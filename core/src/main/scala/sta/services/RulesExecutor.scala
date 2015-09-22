package sta.services

import android.app.Service
import android.os.HandlerThread
import cats.data.Validated
import kj.android.logging.Logging
import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal
import sta.common.Requirement
import sta.model.{BaseModel, Rule}
import sta.model.actions.Action

abstract class RulesExecutor extends Service with Logging {
  private[this] val mainThread = new HandlerThread(s"${logTag.tag}-Main")
  mainThread.start()

  protected[this] val mainLooper = mainThread.getLooper

  private[this] val actionExecutors = TrieMap.empty[Class[_], Action => Option[Throwable]]

  @inline implicit def self = this

  def resetTimers(requirements: Set[Requirement]): Unit

  def updateState(model: BaseModel): Unit

  protected def registerActionExecutor(clazz: Class[_], executor: Action => Option[Throwable]) = {
    actionExecutors += (clazz -> executor)
  }

  def executeAction(action: Action): Rule#Result = {
    actionExecutors.get(action.getClass).map { executor =>
      executor(action).fold[Rule#Result](Validated.valid(()))(t => Validated.invalidNel(action.name -> t))
    }.getOrElse {
      try {
        Validated.valid(action.execute())
      } catch {
        case NonFatal(t) => Validated.invalidNel(action.name -> t)
      }
    }
  }

  override def onDestroy(): Unit = {
    super.onDestroy()

    mainLooper.quit()
  }
}