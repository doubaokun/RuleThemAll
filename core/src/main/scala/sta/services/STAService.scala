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

class STAService extends Service with Logging { ctx =>

  import sta.services.STAService._

  private[this] val store: RulesStorage = new FileRulesStorage(ctx)

  object ServicesMap {
    def apply() = {
      val pm = getPackageManager
      val collected = ServiceMacros.collect
      val services = mutable.Map.empty[String, List[(IntentType, ServiceFragment[Model])]]
      for (wst <- collected if wst.features.forall(pm.hasSystemFeature)) {
        val manual = wst.manual.map(_.toMap).getOrElse(Map.empty[String, Duration])
        wst.actual.reactOn.foreach {
          case intent if manual.isDefinedAt(intent) =>
            services += ((intent, (Manual(manual(intent)),
              wst.actual) :: services.getOrElse(intent, Nil)))
          case intent =>
            services += ((intent, (Automatic,
              wst.actual) :: services.getOrElse(intent, Nil)))
        }
      }
      new ServicesMap(services = services.toMap)
    }

    def empty = new ServicesMap(Map.empty)
  }

  case class ServicesMap private[STAService](services: Map[String, List[(IntentType, ServiceFragment[Model])]]) {
    @inline final private def updateState(model: Model): Unit = {
      import model.companion._
      rawState.update { state =>
        state.+(Key, model)(model.companion.ev)
      }.fold(th => log.error("Error has occurred during updating state", th), state =>
        store.rules.foreach { rule =>
          Task(rule.execute(state)(ctx, logTag)).run(_ => ())
        }
      )
    }

    private[this] val tasks: Map[String, Task[Unit]] = for {
      (intent, service) <- services
      (Manual(interval), sf) <- service 
    } yield {
      sf.getClass.getSimpleName -> Task.schedule(0.seconds, interval) {
        sf(registerReceiver(null, new IntentFilter(intent))).foreach(updateState)
      }
    }

    def run(intent: Intent) = for (
      service <- services.get(intent.getAction);
      (Automatic, sf) <- service;
      model <- sf(intent)
    ) updateState(model)

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

  private[this] val rawWorkers = Atomic(ServicesMap.empty)

  private val requestProcessor = new BroadcastReceiver {
    private def update(set: Set[String])
      (f: (Set[String], String => Unit, ServicesMap) => ServicesMap): Unit = {
      val filters = Set.newBuilder[String]
      rawWorkers.update { workers =>
        workers.stopTasks()
        f(set, filters += _, workers)
      }.fold(th => {
        log.error("Failed to update workers", th)
      }, workers => {
        workers.runTasks()
        registerStateProcessor(filters.result(), unregisterFirst = true)
      })
    }

    private def onAdd(toAdd: Set[String], act: String => Unit, services: ServicesMap) = {
      services.copy(services = services.services.map {
        case (v, xs) if toAdd.contains(v)  =>
          if (xs.exists(_._1 == Automatic)) act(v)
          (v, xs.map { case (t, sf) => (t.resume(), sf) })
        case other =>
          if (other._2.exists(_._1 == Automatic)) act(other._1)
          other
      })
    }

    private def onRemove(toRemove: Set[String], act: String => Unit, services: ServicesMap) = {
      services.copy(services = services.services.map {
        case (v, xs) if toRemove.contains(v) =>
          (v, xs.map { case (t, sf) => (t.suspend(), sf) })
        case other =>
          if (other._2.exists(_._1 == Automatic)) act(other._1)
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
    def onReceive(context: Context, intent: Intent): Unit = rawWorkers.get.run(intent)
  }

  private def registerStateProcessor(intents: Set[String], unregisterFirst: Boolean): Unit = {
    if (unregisterFirst) unregisterReceiver(stateProcessor)
    val stateFilter = new IntentFilter()
    intents.foreach(stateFilter.addAction)
    registerReceiver(stateProcessor, stateFilter)
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    rawWorkers.update(_ => ServicesMap()).fold(th => throw th, _ => ())

    val requestFilter = new IntentFilter()
    requestFilter.addAction(LOAD)
    requestFilter.addAction(UNLOAD)
    registerReceiver(requestProcessor, requestFilter)

    val workers = rawWorkers.get
    registerStateProcessor(workers.services.collect {
      case (v, xs) if xs.exists(_._1 == Automatic)  => v
    }(collection.breakOut), unregisterFirst = false)
    workers.runTasks()

    val state = rawState.get
    for (rule <- store.startupRules) {
      Task(rule.execute(state)(ctx, logTag)).run(_ => ())
    }

    super.onStartCommand(intent, flags, startId)
  }

  override def onDestroy(): Unit = {
    unregisterReceiver(requestProcessor)
    unregisterReceiver(stateProcessor)
    rawWorkers.get.stopTasks()

    super.onDestroy()
  }
}
