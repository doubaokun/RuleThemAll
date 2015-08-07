package sta.services

import android.app.Service
import android.content.{BroadcastReceiver, Context, Intent, IntentFilter}
import android.net.Uri
import android.os.{IBinder, Parcelable}
import android.support.v4.content.WakefulBroadcastReceiver
import java.io.File
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import kj.android.concurrent.{Atomic, Scheduler}
import kj.android.logging.Logging
import scala.concurrent.duration.Duration
import scalaz._
import scalaz.concurrent._
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

  private val store: RulesStorage = new FileRulesStorage(this)

  object ServicesMap {
    def apply() = {
      val pm = getPackageManager
      val collected = ServiceMacros.collect
      val services = collected.foldLeft(Map.empty[IntentType, ServiceFragment[Model]]) {
        case (map, wst) if wst.features.forall(pm.hasSystemFeature) =>
          val manual = wst.manual.map(_.toMap).getOrElse(Map.empty[String, Duration])
          map ++ wst.actual.reactOn.map {
            case intent if manual.isDefinedAt(intent) =>
              Suspended(Manual(intent, manual(intent))) -> wst.actual
            case intent =>
              Suspended(Automatic(intent)) -> wst.actual
          }
        case (map, _) => map
      }
      new ServicesMap(services = services)
    }

    def update(map: ServicesMap)(f: ServicesMap => ServicesMap): ServicesMap = {
      map.stopTasks()
      val newMap = f(map)
      newMap.runTasks()
      newMap
    }
  }

  case class ServicesMap private (services: Map[IntentType, ServiceFragment[Model]]) {
    private val cancel = new AtomicBoolean(false)

    private val tasks: Map[IntentType, Task[Unit]] = services.keysIterator.collect {
      case (m @ Manual(intent, interval)) =>
        (m, Scheduler.schedule(
          stateProcessor.onReceive(ctx, registerReceiver(null, new IntentFilter(intent))),
          Duration(0, TimeUnit.SECONDS),
          interval
        ))
    }.toMap

    def stopTasks(): Unit = {
      cancel.set(true)
    }

    def runTasks(): Unit = {
      tasks.foreach {
        case (k, t) => t.runAsyncInterruptibly(_.fold(
          th => log.error(s"Task $k ended with failure", th),
          _ => log.debug(s"Task $k ended with success")
        ), cancel)
      }
    }
  }

  def onBind(intent: Intent): IBinder = null

  private val state = Atomic(HMap.empty[ModelKV])

  private val workers = Atomic(ServicesMap())

  private val requestProcessor = new BroadcastReceiver {
    // runs on main thread
    private def onAdd(toAdd: Set[String]): Unit = {
      val filters = Set.newBuilder[String]
      workers.update { worker =>
        val newServices = worker.services.map {
          case (Suspended(m @ Manual(v, _)), sf) if toAdd.contains(v) =>
            (m, sf)
          case (Suspended(a @ Automatic(v)), sf) if toAdd.contains(v) =>
            filters += v
            (a, sf)
          case ( a @ Automatic(v), sf) =>
            filters += v
            (a, sf)
          case other =>
            other
        }
        ServicesMap.update(worker)(_.copy(services = newServices))
      }
      registerStateProcessor(filters.result(), unregisterFirst = true)
    }

    private def onRemove(toRemove: Set[String]): Unit = {
      val filters = Set.newBuilder[String]
      workers.update { worker =>
        val newServices = worker.services.map {
          case (n @ Automatic(v), sf) if toRemove.contains(v) =>
            (Suspended(n), sf)
          case (m @ Manual(v, _), sf) if toRemove.contains(v) =>
            (Suspended(m), sf)
          case (n @ Automatic(v), sf) =>
            filters += v
            (n, sf)
          case other =>
            other
        }
        ServicesMap.update(worker)(_.copy(services = newServices))
      }
      registerStateProcessor(filters.result(), unregisterFirst = true)
    }

    def onReceive(context: Context, intent: Intent): Unit = {
      intent.getAction match {
        case LOAD => intent.extra[Array[Uri]].get(URI).foreach { path =>
          onAdd(store.register(new File(path.getPath)))
        }
        case UNLOAD => onRemove(store.unregister(intent.extra[Array[String]].get(NAME): _*))
        case other => log.warn(s"Unknown intent received: $other")
      }
    }
  }

  private val stateProcessor = new WakefulBroadcastReceiver {
    // runs on main thread
    def onReceive(context: Context, intent: Intent): Unit = {
      implicit val ctx = context

      workers.get.services.get(Automatic(intent.getAction)).foreach { service =>
        \/.fromTryCatchNonFatal(service.handle(intent)).fold(
          th => log.error("Error has occurred during handling incoming intent", th),
          model => state.update { map =>
            import model.companion._
            map + (Key -> model)
          }.fold(
            th =>
              log.error("Error has occurred during updating state", th),
            map =>
              store.rules.foreach { d =>
                Task(d.execute(map)).runAsync(_.foreach(_.fold(
                  _ => (),
                  v => {
                    v.leftMap(_.foreach {
                      case (name, th) =>
                        log.error(s"Error has occurred during running action $name in ${d.name}", th)
                    })
                    ()
                  }
                )))
              }
          )
        )
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

    val w = workers.get
    registerStateProcessor(w.services.keysIterator.collect {
      case Automatic(v) => v
    }.toSet, unregisterFirst = false)
    w.runTasks()

    super.onStartCommand(intent, flags, startId)
  }

  override def onDestroy(): Unit = {
    unregisterReceiver(requestProcessor)
    unregisterReceiver(stateProcessor)
    workers.get.stopTasks()
    super.onDestroy()
  }

  override def onCreate(): Unit = {
    // TODO: check loaded triggers
    super.onCreate()
  }
}
