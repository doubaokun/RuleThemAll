package sta.services

import android.app.Service
import android.content.{BroadcastReceiver, Context, Intent, IntentFilter}
import android.net.Uri
import android.os.{IBinder, Parcelable}
import android.support.v4.content.WakefulBroadcastReceiver
import java.io.File
import kj.android.concurrent._
import kj.android.logging.Logging
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.control.NonFatal
import shapeless.HMap
import sta.model.{Model, ModelKV}
import sta.storage.{FileRulesStorage, RulesStorage}

object STAService {
  val URI = "path"
  val LOAD = "load"

  val NAME = "name"
  val UNLOAD = "unload"

  def loadRules(from: Uri*): Intent = {
    val intent = new Intent(LOAD)
    intent.putExtra(URI, from.toArray[Parcelable])
    intent
  }

  def unloadRules(names: String*): Intent = {
    val intent = new Intent(UNLOAD)
    intent.putExtra(NAME, names.toArray)
    intent
  }

  sealed abstract class IntentType

  case class Automatic(intent: String) extends IntentType {
    override def hashCode(): Int = intent.hashCode
  }

  case class Manual(intent: String, interval: Duration) extends IntentType {
    override def hashCode(): Int = intent.hashCode
  }

  case class Suspended(underlying: IntentType) extends IntentType
}

class STAService extends Service with Logging { ctx =>

  import sta.services.STAService._

  private[this] val store: RulesStorage = new FileRulesStorage(ctx)

  object ServicesMap {
    def apply() = {
      val pm = getPackageManager
      val collected = ServiceMacros.collect
      val services = mutable.Map.empty[IntentType, List[ServiceFragment[Model]]]
      for (wst <- collected if wst.features.forall(pm.hasSystemFeature)) {
        val manual = wst.manual.map(_.toMap).getOrElse(Map.empty[String, Duration])
        wst.actual.reactOn.foreach {
          case intent if manual.isDefinedAt(intent) =>
            val tpe = Suspended(Manual(intent, manual(intent)))
            services += (tpe -> (wst.actual :: services.getOrElse(tpe, Nil)))
          case intent =>
            val tpe = Suspended(Automatic(intent))
            services += (tpe -> (wst.actual :: services.getOrElse(tpe, Nil)))
        }
      }
      new ServicesMap(services = services.toMap)
    }
  }

  case class ServicesMap private (services: Map[IntentType, List[ServiceFragment[Model]]]) {
    private[this] val tasks: Map[IntentType, Task[Unit]] = services.keysIterator.collect {
      case (m @ Manual(intent, interval)) =>
        (m, Task.schedule(0.seconds, interval) {
          stateProcessor.onReceive(ctx, registerReceiver(null, new IntentFilter(intent)))
        })
    }.toMap

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

  def onBind(intent: Intent): IBinder = null

  private[this] val rawState = Atomic(HMap.empty[ModelKV])

  private[this] val rawWorkers = Atomic(ServicesMap())

  private val requestProcessor = new BroadcastReceiver {
    private def update(set: Set[String])
      (f: (Set[String], String => Unit, ServicesMap) => ServicesMap): Unit = {
      val filters = Set.newBuilder[String]
      rawWorkers.update { workers =>
        workers.stopTasks()
        f(set, filters += _, workers)
      }.fold(th => {

      }, workers => {
        workers.runTasks()
        registerStateProcessor(filters.result(), unregisterFirst = true)
      })
    }

    private def onAdd(toAdd: Set[String], act: String => Unit, services: ServicesMap) = {
      services.copy(services = services.services.map {
        case (Suspended(m @ Manual(v, _)), sf) if toAdd.contains(v) =>
          (m, sf)
        case (Suspended(a @ Automatic(v)), sf) if toAdd.contains(v) =>
          act(v)
          (a, sf)
        case (a@Automatic(v), sf) =>
          act(v)
          (a, sf)
        case other =>
          other
      })
    }

    private def onRemove(toRemove: Set[String], act: String => Unit, services: ServicesMap) = {
      services.copy(services = services.services.map {
        case (n @ Automatic(v), sf) if toRemove.contains(v) =>
          (Suspended(n), sf)
        case (m @ Manual(v, _), sf) if toRemove.contains(v) =>
          (Suspended(m), sf)
        case (n @ Automatic(v), sf) =>
          act(v)
          (n, sf)
        case other =>
          other
      })
    }

    def onReceive(context: Context, intent: Intent): Unit = {
      intent.getAction match {
        case LOAD => intent.extra[Array[Uri]].get(URI).foreach { path =>
          update(store.register(new File(path.getPath)))(onAdd)
        }
        case UNLOAD => update(store.unregister(intent.extra[Array[String]].get(NAME): _*))(onRemove)
        case other => log.warn(s"Unknown intent received: $other")
      }
    }
  }

  private val stateProcessor = new WakefulBroadcastReceiver {
    def onReceive(context: Context, intent: Intent): Unit = {
      for (
        services <- rawWorkers.get.services.get(Automatic(intent.getAction));
        service <- services
      ) {
        try {
          for(model <- service.handle(intent)) {
            import model.companion._
            rawState.update { state =>
              state + (Key -> model)
            }.fold(th => log.error("Error has occurred during updating state", th), state =>
              store.rules.foreach { rule =>
                Task(rule.execute(state)(ctx, logTag)).run(_ => ())
              }
            )
          }
        } catch {
          case NonFatal(th) => log.error("Error has occurred during handling incoming intent", th)
        }
      }
    }
  }

  private def registerStateProcessor(intents: Set[String], unregisterFirst: Boolean): Unit = {
    if (unregisterFirst) unregisterReceiver(stateProcessor)
    val stateFilter = new IntentFilter()
    intents.foreach(stateFilter.addAction)
    registerReceiver(stateProcessor, stateFilter)
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    val requestFilter = new IntentFilter()
    requestFilter.addAction(LOAD)
    requestFilter.addAction(UNLOAD)
    registerReceiver(requestProcessor, requestFilter)

    val w = rawWorkers.get
    registerStateProcessor(w.services.keysIterator.collect {
      case Automatic(v) => v
    }.toSet, unregisterFirst = false)
    w.runTasks()

    super.onStartCommand(intent, flags, startId)
  }

  override def onDestroy(): Unit = {
    unregisterReceiver(requestProcessor)
    unregisterReceiver(stateProcessor)
    rawWorkers.get.stopTasks()

    super.onDestroy()
  }

  override def onCreate(): Unit = {
    val state = rawState.get
    for (rule <- store.startupRules) {
      Task(rule.execute(state)(ctx, logTag)).run(_ => ())
    }

    super.onCreate()
  }
}
