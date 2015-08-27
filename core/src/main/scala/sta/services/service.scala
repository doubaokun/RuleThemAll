package sta.services

import android.app.{PendingIntent, Service}
import android.content.{BroadcastReceiver, Context, Intent}
import android.net.Uri
import android.os._
import android.support.v4.content.WakefulBroadcastReceiver
import java.io.File
import kj.android.common.{AppInfo, Notify}
import kj.android.concurrent._
import kj.android.logging.Logging
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Failure
import shapeless.HMap
import sta.common.Requirement
import sta.model.{Model, ModelKV}
import sta.storage.PlaintextStorage

class BootReceiver extends BroadcastReceiver with Logging {
  def onReceive(context: Context, intent: Intent): Unit = intent.getAction match {
    case Intent.ACTION_BOOT_COMPLETED =>
      val clazz = classOf[STAService]
      log.info(s"Starting ${clazz.getSimpleName}")
      context.startService(new Intent(context, clazz))
    case other =>
      log.warn(s"Unknown message received: $other")
  }
}

object STAService {
  val ACTION = "action"

  val URI = "path"
  val LOAD = 1

  val NAME = "name"
  val UNLOAD = 2

  def loadRules(from: Uri*): Message = {
    val bundle = new Bundle()
    bundle.putStringArray(URI, from.map(_.getPath)(collection.breakOut))
    val msg = Message.obtain(null, LOAD)
    msg.setData(bundle)
    msg
  }

  def unloadRules(names: String*): Message = {
    val bundle = new Bundle()
    bundle.putStringArray(URI, names.toArray)
    val msg = Message.obtain(null, UNLOAD)
    msg.setData(bundle)
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

class STAService extends Service with TriggerExecutor with Logging { root =>

  import sta.services.STAService._

  @inline implicit def ctx: Context = this

  type SF = List[(IntentType, ServiceFragment[Model])]

  class ServicesMap private[STAService](rawMap: Map[Int, (Requirement, SF)]) {
    @inline private final def updateModel(model: Model): Unit = {
      import model.companion._
      updateState(_ + (Key -> model))
    }

    private[this] val tasks: Map[String, Task[Unit]] = for {
      (_, (Requirement.IntentBased(intent), service)) <- rawMap
      (Manual(interval), sf) <- service 
    } yield {
      sf.logTag.tag -> Task.schedule(0.seconds, interval) {
        sf(ctx, registerReceiver(null, intent)).foreach(updateModel)
      }
    }
    
    private[this] val runnable = {
      val automatic = mutable.Map.empty[Int, List[ServiceFragment[Model]]]
      for (
        (hash, (intent, service)) <- rawMap;
        (Automatic, sf) <- service
      ) {
        automatic += (hash -> (sf :: automatic.getOrElse(hash, Nil)))
      }
      automatic
    }

    def iterator: Iterator[(Requirement, SF)] = rawMap.valuesIterator
    
    def map(f: ((Requirement, SF)) => (Requirement, SF)): ServicesMap = {
      new ServicesMap(rawMap.map { case (k, v) =>
        val nv = f(v)
        nv._1.hashCode() -> nv
      })
    }

    def run(context: Context, intent: Intent) = for (
      services <- runnable.get(intent.filterHashCode());
      sf <- services;
      model <- sf(context, intent)
    ) {
      updateModel(model)
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
      new ServicesMap(rawMap = services.map { case (k, v) => (k.hashCode(), (k, v)) }(collection.breakOut))
    }
  }

  def onBind(intent: Intent): IBinder = requestProcessor.getBinder

  private[this] implicit lazy val appInfo = AppInfo(
    name = ctx.getResources.getString(ctx.getApplicationInfo.labelRes),
    smallIcon = ctx.getApplicationInfo.icon
  )

  private[this] lazy val storage = new PlaintextStorage

  private[this] lazy val rawServices = Atomic(ServicesMap())

  private[this] val rawState = Atomic(HMap.empty[ModelKV])

  private[this] val requestProcessor = new Messenger(new Handler {
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

    override def handleMessage(msg: Message): Unit = {
      msg.what match {
        case LOAD => msg.getData.getStringArray(URI).foreach { path =>
          val (toAdd, toRemove) = storage.register(new File(path))
          if (toAdd.nonEmpty || toRemove.nonEmpty) update(onAdd(toAdd, toRemove))
        }
        case UNLOAD =>
          val toRemove = storage.unregister(msg.getData.getStringArray(NAME): _*)
          if (toRemove.nonEmpty) update(onRemove(toRemove))
        case other => log.warn(s"Unknown message received: $other")
      }
    }
  })

  private[this] val stateProcessor = new WakefulBroadcastReceiver {
    def onReceive(context: Context, intent: Intent): Unit = rawServices.get.run(context, intent)
  }

  private def registerStateProcessor(intents: Seq[Intent], unregisterFirst: Boolean): Unit = {
    if (unregisterFirst) unregisterReceiver(stateProcessor)
    intents.foreach(registerReceiver(stateProcessor, _))
  }

  def state: HMap[ModelKV] = rawState.get

  def updateState(f: HMap[ModelKV] => HMap[ModelKV]): Unit = {
    rawState.update(f).fold(th => log.error("Error has occurred during updating state", th), state => {
      storage.rules.foreach { rule =>
        Task(rule.execute(state)).run(_ => ())
      }
    })
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    appInfo
    storage
    startForeground(1, Notify.build(s"Monitoring",
      intent.extra[PendingIntent].get(STAService.ACTION)))
    val services = rawServices.get
    registerStateProcessor(services.iterator.collect {
      case (Requirement.IntentBased(v), xs) if xs.exists(_._1 == Automatic)  => v
    }.toSeq, unregisterFirst = false)
    services.runTasks()

    val state = rawState.get
    for (rule <- storage.startupRules) {
      Task(rule.execute(state)).run(_ => ())
    }

    Service.START_STICKY
  }

  override def onDestroy(): Unit = {
    stopForeground(true)
    unregisterReceiver(stateProcessor)
    rawServices.get.stopTasks()
  }
}
