package rta.plugin

import android.app.Service
import android.content.Intent
import android.os._
import java.util
import java.util.concurrent.locks.ReentrantReadWriteLock
import rta.common.{Common, Requirement, Utils}
import rta.logging.Logging
import rta.model.BaseModel
import rta.model.actions.Action
import rta.parser.{ActionParser, TriggerParser}
import scala.collection.mutable
import scala.util.control.NonFatal

class Plugin[A <: Action, M <: BaseModel] extends Service with Common with Logging { ctx =>
  private[this] val pluginLock = new ReentrantReadWriteLock

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


    def register(onNewModel: OnNewModel, onResetTimers: OnResetTimers): Unit =
      Utils.inLock(pluginLock.writeLock()) {
        updaters += (onNewModel -> onResetTimers)
      }
  }

  final def onBind(intent: Intent): IBinder = binder

  final def resetTimers(req1: Requirement, reqs: Requirement*): Unit = {
    val list = new util.ArrayList[RemoteObject](1)
    list.add(new RemoteObject(req1))
    reqs.foreach(r => list.add(new RemoteObject(r)))
    Utils.inLock(pluginLock.readLock()) {
      updaters.foreach { case (_, update) =>
        update.onResetTimers(list)
      }
    }
  }

  final def update(model: M): Unit = {
    val single = new RemoteObject(model)
    Utils.inLock(pluginLock.readLock()) {
      updaters.foreach { case (update, _) =>
        update.onNewModel(single)
      }
    }
  }

  def actionParser: Option[ActionParser[A]] = None

  def triggerParser: Option[TriggerParser[M]] = None
}

object Plugin {
  val CATEGORY = "rta.plugin"
}
