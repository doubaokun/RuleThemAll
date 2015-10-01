package sta.plugin

import android.app.Service
import android.content.Intent
import android.os._
import java.util
import scala.collection.mutable
import scala.util.control.NonFatal
import sta.common.{Common, Requirement}
import sta.logging.Logging
import sta.model.BaseModel
import sta.model.actions.Action
import sta.parser.{ActionParser, TriggerParser}

class Plugin[A <: Action, M <: BaseModel] extends Service with Common with Logging { ctx =>
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.MutableDataStructures"))
  private[this] val updaters = mutable.ArrayBuffer.empty[(OnNewModel, OnResetTimers)]

  private[this] val binder = new IPlugin.Stub() {
    def actionParser(): RemoteObject = new RemoteObject(ctx.actionParser)

    def triggerParser(): RemoteObject = new RemoteObject(ctx.triggerParser)

    def executeAction(action: RemoteObject): RemoteObject = new RemoteObject(try {
      action.as[A].execute()(ctx)
      None
    } catch {
      case NonFatal(th) => Some(th)
    })

    def register(onNewModel: OnNewModel, onResetTimers: OnResetTimers): Unit = updaters.synchronized {
      updaters += (onNewModel -> onResetTimers)
    }
  }

  final def onBind(intent: Intent): IBinder = binder

  final def resetTimers(req1: Requirement, reqs: Requirement*): Unit = {
    val list = new util.ArrayList[RemoteObject](1)
    list.add(new RemoteObject(req1))
    reqs.foreach(r => list.add(new RemoteObject(r)))
    updaters.foreach { case (_, update) =>
      update.onResetTimers(list)
    }
  }

  final def update(model: M): Unit = {
    val single = new RemoteObject(model)
    updaters.foreach { case (update, _) =>
      update.onNewModel(single)
    }
  }

  def actionParser: Option[ActionParser[A]] = None

  def triggerParser: Option[TriggerParser[M]] = None
}

object Plugin {
  val CATEGORY = "sta.plugin"
}
