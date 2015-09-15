package sta.services

import android.app.Service
import android.content.Context
import android.os.HandlerThread
import kj.android.concurrent.ExecutionContext
import kj.android.logging.Logging
import sta.common.Requirement

abstract class RulesExecutor extends Service with Logging {
  private[this] val handlerThread = new HandlerThread(logTag.tag)
  handlerThread.start()

  protected[this] val looper = handlerThread.getLooper

  @inline implicit def ctx: Context = this

  @inline implicit def ec = ExecutionContext.fromHandlerThread(handlerThread)

  def resetTimers(requirements: Set[Requirement]): Unit

  override def onDestroy(): Unit = {
    super.onDestroy()

    looper.quit()
  }
}
