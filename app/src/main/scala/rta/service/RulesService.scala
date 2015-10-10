package rta.service

import android.app.{PendingIntent, Service}
import android.content._
import android.net.Uri
import android.os._
import java.io.File
import rta.common.{Utils, AppInfo, Notify, Requirement}
import rta.concurrent.ExecutionContext.Implicits._
import rta.concurrent.{Atomic, Task}
import rta.model.BaseModel
import rta.model.triggers.Trigger
import rta.storage.PlaintextStorage
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.control.NonFatal

object RulesService {
  val BACKGROUND_ACTION = "rta.background_action"

  val LOAD = 1

  val UNLOAD = 2

  val LIST = 3
  val SUBSCRIBE = 4
  val UNSUBSCRIBE = 5
  val NAMES = "rta.rule.name"

  def loadRules(from: Uri*): Message = {
    val msg = Message.obtain(null, LOAD)
    msg.obj = from.map(_.getPath).toArray
    msg
  }

  def unloadRules(names: String*): Message = {
    val msg = Message.obtain(null, UNLOAD)
    msg.obj = names.toArray
    msg
  }

  sealed abstract class IntentType {
    def suspend(): Suspended = Suspended(this)

    def resume(): IntentType = this
  }

  case object Automatic extends IntentType

  case class Manual(interval: Duration) extends IntentType

  case class Suspended(underlying: IntentType) extends IntentType {
    override def suspend(): Suspended = this

    override def resume(): IntentType = underlying
  }
}

class RulesService extends RulesExecutor with PluginHandler { root =>

  import RulesService._

  private[this] val handler = new Handler(mainLooper) {
    private[this] val subscribers = mutable.Set.empty[Messenger]

    private def update(f: (Intent => Unit, (Intent, SF)) => (Intent, SF)): Unit = {
      val intents = Seq.newBuilder[Intent]
      rawServices.update { services =>
        services.stopTasks()
        services.map {
          case (Requirement.IntentBased(intent), sf) =>
            val (newIntent, newSF) = f(intents += _, intent -> sf)
            Requirement.IntentBased(newIntent) -> newSF
          case other => other
        }
      }.fold(th => {
        log.error("Failed to update services", th)
        throw th
      }, services => {
        services.runTasks()
        registerStateProcessor(intents.result(), unregisterFirst = true)
      })
    }

    private def onAdd(toAdd: Set[Int], toRemove: Set[Int])(act: Intent => Unit, kv: (Intent, SF)) = {
      val hash = kv._1.filterHashCode()
      kv match {
        case (v, xs) if toAdd.contains(hash)  =>
          val mapped = xs.map { case (t, sf) => (t.resume(), sf) }
          if (mapped.exists(_._1 == Automatic)) act(v)
          (v, mapped)
        case (v, xs) if toRemove.contains(hash) =>
          (v, xs.map { case (t, sf) => (t.suspend(), sf) })
        case other =>
          if (other._2.exists(_._1 == Automatic)) act(other._1)
          other
      }
    }

    private def onRemove(toRemove: Set[Int])(act: Intent => Unit, kv: (Intent, SF)) = {
      kv match {
        case (v, xs) if toRemove.contains(v.filterHashCode()) =>
          (v, xs.map { case (t, sf) => (t.suspend(), sf) })
        case other =>
          if (other._2.exists(_._1 == Automatic)) act(other._1)
          other
      }
    }

    private def rulesMsg(names: Array[String]) = {
      val reply = Message.obtain(null, LIST)
      val bundle = new Bundle
      bundle.putStringArray(NAMES, names)
      reply.setData(bundle)
      reply
    }

    override def handleMessage(msg: Message): Unit = try {
      msg.what match {
        case LOAD =>
          val files: List[File] = msg.obj.asInstanceOf[Array[String]].map(new File(_))(collection.breakOut)
          log.info(s"Loading rules from $files")
          val info = storage.register(files: _*)
          info.addedRules.foreach(timers += _)
          if (info.addedRequirements.nonEmpty || info.removedRequirements.nonEmpty)
            update(onAdd(info.addedRequirements, info.removedRequirements))
          if (subscribers.nonEmpty) {
            val all = storage.allRules.map(_.name).toArray
            for (subscriber <- subscribers) {
              Utils.remote(subscribers -= subscriber) {
                subscriber.send(rulesMsg(all))
              }
            }
          }
          rawState.reset()
        case UNLOAD =>
          val ruleNames: List[String] = msg.obj.asInstanceOf[Array[String]].toList
          log.info(s"Unloading rules $ruleNames")
          val toRemove = storage.unregister(ruleNames: _*)
          ruleNames.foreach(timers -= _)
          if (toRemove.nonEmpty) update(onRemove(toRemove))
          if (subscribers.nonEmpty) {
            val all = storage.allRules.map(_.name).toArray
            for (subscriber <- subscribers) {
              Utils.remote(subscribers -= subscriber) {
                subscriber.send(rulesMsg(all))
              }
            }
          }
          rawState.reset()
        case LIST if msg.replyTo != null =>
          msg.replyTo.send(rulesMsg(storage.allRules.map(_.name).toArray))
        case SUBSCRIBE if msg.replyTo != null =>
          subscribers += msg.replyTo
          msg.replyTo.send(rulesMsg(storage.allRules.map(_.name).toArray))
        case UNSUBSCRIBE if msg.replyTo != null =>
          subscribers -= msg.replyTo
        case other =>
          log.warn(s"Unknown message received: $other")
      }
    } catch {
      case NonFatal(th) => log.error(s"Exception has occurred during handling message: $msg", th)
    }
  }

  type SF = List[(IntentType, ServiceFragment[BaseModel])]

  class ServicesMap private[RulesService](rawMap: Map[Requirement, SF]) {
    private[this] val tasks: Map[String, Task[Unit]] = for {
      (Requirement.IntentBased(intent), service) <- rawMap
      (Manual(interval), sf) <- service
    } yield {
      sf.logTag.tag -> Task.schedule(0.seconds, interval) {
        sf(registerReceiver(null, intent, null, handler)).foreach(updateState)
      }
    }

    private[this] val runnable = {
      val automatic = mutable.Map.empty[Int, List[ServiceFragment[BaseModel]]]
      for (
        (req, service) <- rawMap;
        (Automatic, sf) <- service
      ) {
        val hash = req.hashCode()
        automatic += (hash -> (sf :: automatic.getOrElse(hash, Nil)))
      }
      automatic
    }

    def iterator: Iterator[(Requirement, SF)] = rawMap.iterator

    def map(f: ((Requirement, SF)) => (Requirement, SF)): ServicesMap = {
      new ServicesMap(rawMap.map(f))
    }

    def run(intent: Intent) = Task.runWithWakeLock {
      for (
        services <- runnable.get(intent.filterHashCode());
        sf <- services;
        model <- sf(intent)
      ) {
        updateState(model)
      }
    }

    def runAllTasks(forRequirements: Set[Requirement])(implicit context: Context) = for (
      (req @ Requirement.IntentBased(intent), services) <- rawMap if forRequirements.contains(req);
      (_, sf) <- services
    ) {
      sf(registerReceiver(null, intent, null, handler)).foreach(updateState)
    }

    def stopTasks(): Boolean = {
      tasks.valuesIterator.forall(_.cancel(true))
    }

    def runTasks(): Unit = {
      tasks.foreach {
        case (k, t) =>
          t.run {
            case Failure(th) => log.error(s"Task $k ended with failure", th)
            case _ => log.debug(s"Task $k ended with success")
          }
      }
    }
  }

  object ServicesMap {
    def apply() = {
      val pm = getPackageManager
      val collected = ServiceMacros.collect(root)
      val services = mutable.Map.empty[Requirement, SF]
      val rules = storage.rules.flatMap(_.requires).toSet
      for (wst <- collected if wst.uses.features.forall(pm.hasSystemFeature)) {
        val manual = wst.manual.map(_.toMap).getOrElse(Map.empty[String, Duration])
        wst.uses.requirements.foreach {
          case requirement @ Requirement.IntentBased(intent) if manual.isDefinedAt(intent.getAction) =>
            services += ((requirement, (
              if (!rules.contains(requirement)) Manual(manual(intent.getAction)).suspend()
              else Manual(manual(intent.getAction)),
              wst.actual
            ) :: services.getOrElse(requirement, Nil)))
          case requirement =>
            services += ((requirement, (
              if (!rules.contains(requirement)) Automatic.suspend() else Automatic,
              wst.actual
            ) :: services.getOrElse(requirement, Nil)))
        }
      }
      new ServicesMap(rawMap = services.toMap)
    }
  }

  private[this] implicit lazy val appInfo = AppInfo(
    name = getResources.getString(getApplicationInfo.labelRes),
    smallIcon = getApplicationInfo.icon
  )

  private[this] implicit lazy val storage = new PlaintextStorage

  private[this] lazy val rawServices = Atomic(ServicesMap())

  private[this] lazy val timers = new TimerMap

  private[this] lazy val rawState = new RulesState

  private[this] var started = false

  private[this] val requestProcessor = new Messenger(handler)

  private[this] val stateProcessor = new BroadcastReceiver {
    def onReceive(context: Context, intent: Intent): Unit = rawServices.get.run(intent)
  }

  private def registerStateProcessor(intents: Seq[Intent], unregisterFirst: Boolean): Unit = {
    if (unregisterFirst) try {
      unregisterReceiver(stateProcessor)
    } catch {
      case ex: IllegalArgumentException =>
    }
    intents.foreach(registerReceiver(stateProcessor, _, null, handler))
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    if (started) {
      intent match {
        case Trigger.Timer(ruleName, branchId, partial) =>
          log.info(s"Handling timer $intent")
          Task.runWithWakeLock {
            for (rule <- storage.get(ruleName)) {
              rawServices.get.runAllTasks(rule.requires)
              rule.executeBranch(branchId, intent, rawState.snapshot, !partial)
            }
          }
        case _ =>
          log.warn(s"Unknown $intent", new RuntimeException)
      }
    } else {
      log.info("Starting service")

      try { // startForeground does not work with android instrumentation tests
        startForeground(1, Notify.build(s"Monitoring",
          intent.get[PendingIntent](RulesService.BACKGROUND_ACTION)))
      } catch {
        case ex: NullPointerException =>
      }
      started = true
    }

    Service.START_STICKY
  }

  override def onCreate(): Unit = {
    super.onCreate()

    appInfo
    storage
    timers

    val services = rawServices.get
    registerStateProcessor(services.iterator.collect {
      case (Requirement.IntentBased(v), xs) if xs.exists(_._1 == Automatic) => v
    }.toSeq, unregisterFirst = false)
    services.runTasks()

    Task {
      storage.rules.foreach(timers += _)
      storage.startupRules.foreach(_.execute)
    }.run(_ => ())
  }

  override def onDestroy(): Unit = {
    super.onDestroy()

    timers.cancelAll()
    rawServices.get.stopTasks()
    try {
      unregisterReceiver(stateProcessor)
      stopForeground(true)
      started = false
    } catch {
      case ex: IllegalArgumentException =>
      case ex: NullPointerException => // stopForeground does not work with android instrumentation tests
    }
  }

  def onBind(intent: Intent): IBinder = requestProcessor.getBinder

  def resetTimers(requirements: Set[Requirement]): Unit = requirements.foreach(timers.reset)

  def updateState(model: BaseModel) = rawState.update(model).foreach(_.execute)
}
