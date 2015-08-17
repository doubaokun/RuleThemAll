package sta.services

import android.app.Service
import android.content.{BroadcastReceiver, Context, Intent, IntentFilter}
import android.net.Uri
import android.os._
import android.support.v4.content.WakefulBroadcastReceiver
import java.io.File
import kj.android.common.AppInfo
import kj.android.concurrent._
import kj.android.logging.Logging
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Failure
import shapeless.HMap
import sta.model.{Model, ModelKV}
import sta.storage.PlaintextStorage

class BootReceiver extends BroadcastReceiver with Logging {
  def onReceive(context: Context, intent: Intent): Unit = intent.getAction match {
    case Intent.ACTION_BOOT_COMPLETED => context.startService(new Intent(context, classOf[STAService]))
    case other => log.warn(s"Unknown message received: $other")
  }
}

object STAService {
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

class STAService extends Service with Logging {

  import sta.services.STAService._

  @inline implicit def ctx: Context = this

  type SF = List[(IntentType, ServiceFragment[Model])]

  case class ServicesMap private[STAService](map: Map[String, SF]) {
    @inline final private def updateState(model: Model): Unit = {
      import model.companion._
      rawState.update { state =>
        state + (Key -> model)
      }.fold(th => log.error("Error has occurred during updating state", th), state => {
        store.rules.foreach { rule =>
          Task(rule.execute(state)).run(_ => ())
        }
      })
    }

    private[this] val tasks: Map[String, Task[Unit]] = for {
      (intent, service) <- map
      (Manual(interval), sf) <- service 
    } yield {
      sf.logTag.tag -> Task.schedule(0.seconds, interval) {
        sf(ctx, registerReceiver(null, new IntentFilter(intent))).foreach(updateState)
      }
    }
    
    private[this] val runnable = {
      val automatic = mutable.Map.empty[String, List[ServiceFragment[Model]]]
      for (
        (intent, service) <- map;
        (Automatic, sf) <- service
      ) {
        automatic += (intent -> (sf :: automatic.getOrElse(intent, Nil)))
      }
      automatic
    }

    def run(context: Context, intent: Intent) = for (
      services <- runnable.get(intent.getAction);
      sf <- services;
      model <- sf(context, intent)
    ) {
      updateState(model)
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
      val collected = ServiceMacros.collect
      val services = mutable.Map.empty[String, SF]
      for (wst <- collected if wst.features.features.forall(pm.hasSystemFeature)) {
        val manual = wst.manual.map(_.toMap).getOrElse(Map.empty[String, Duration])
        wst.features.intents.foreach {
          case intent if manual.isDefinedAt(intent) =>
            services += ((intent, (Manual(manual(intent)).suspend(),
              wst.actual) :: services.getOrElse(intent, Nil)))
          case intent =>
            services += ((intent, (Automatic.suspend(),
              wst.actual) :: services.getOrElse(intent, Nil)))
        }
      }
      new ServicesMap(map = services.toMap)
    }
  }

  def onBind(intent: Intent): IBinder = requestProcessor.getBinder

  private[this] implicit lazy val appInfo = AppInfo(
    name = ctx.getResources.getString(ctx.getApplicationInfo.labelRes),
    smallIcon = ctx.getApplicationInfo.icon
  )

  private[this] lazy val store = new PlaintextStorage

  private[this] lazy val rawServices = Atomic(ServicesMap())

  private[this] val rawState = Atomic(HMap.empty[ModelKV])

  private val requestProcessor = new Messenger(new Handler {
    private def update(f: (String => Unit, (String, SF)) => (String, SF)): Unit = {
      val filters = Set.newBuilder[String]
      rawServices.update { services =>
        services.stopTasks()
        services.copy(map = services.map.map(f(filters += _, _)))
      }.fold(th => {
        log.error("Failed to update services", th)
      }, services => {
        services.runTasks()
        registerStateProcessor(filters.result(), unregisterFirst = true)
      })
    }

    private def onAdd(toAdd: Set[String], toRemove: Set[String])(act: String => Unit, kv: (String, SF)) = {
      kv match {
        case (v, xs) if toAdd.contains(v)  =>
          val mapped = xs.map { case (t, sf) => (t.resume(), sf) }
          if (mapped.exists(_._1 == Automatic)) act(v)
          (v, mapped)
        case (v, xs) if toRemove.contains(v) =>
          (v, xs.map { case (t, sf) => (t.suspend(), sf) })
        case other =>
          if (other._2.exists(_._1 == Automatic)) act(other._1)
          other
      }
    }

    private def onRemove(toRemove: Set[String])(act: String => Unit, kv: (String, SF)) = {
      kv match {
        case (v, xs) if toRemove.contains(v) =>
          (v, xs.map { case (t, sf) => (t.suspend(), sf) })
        case other =>
          if (other._2.exists(_._1 == Automatic)) act(other._1)
          other
      }
    }

    override def handleMessage(msg: Message): Unit = {
      msg.what match {
        case LOAD => msg.getData.getStringArray(URI).foreach { path =>
          val (toAdd, toRemove) = store.register(new File(path))
          if (toAdd.nonEmpty || toRemove.nonEmpty) update(onAdd(toAdd, toRemove))
        }
        case UNLOAD =>
          val toRemove = store.unregister(msg.getData.getStringArray(NAME): _*)
          if (toRemove.nonEmpty) update(onRemove(toRemove))
        case other => log.warn(s"Unknown message received: $other")
      }
    }
  })

  private val stateProcessor = new WakefulBroadcastReceiver {
    def onReceive(context: Context, intent: Intent): Unit = rawServices.get.run(context, intent)
  }

  private def registerStateProcessor(intents: Set[String], unregisterFirst: Boolean): Unit = {
    if (unregisterFirst) unregisterReceiver(stateProcessor)
    val stateFilter = new IntentFilter()
    intents.foreach(stateFilter.addAction)
    registerReceiver(stateProcessor, stateFilter)
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    appInfo
    store

    val services = rawServices.get
    registerStateProcessor(services.map.collect {
      case (v, xs) if xs.exists(_._1 == Automatic)  => v
    }(collection.breakOut), unregisterFirst = false)
    services.runTasks()

    val state = rawState.get
    for (rule <- store.startupRules) {
      Task(rule.execute(state)).run(_ => ())
    }

    super.onStartCommand(intent, flags, startId)
  }

  override def onDestroy(): Unit = {
    unregisterReceiver(stateProcessor)
    rawServices.get.stopTasks()

    super.onDestroy()
  }
}
