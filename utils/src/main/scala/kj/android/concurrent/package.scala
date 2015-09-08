package kj.android

import android.os.HandlerThread
import java.util.concurrent.{ScheduledThreadPoolExecutor, ScheduledExecutorService, Executors, ThreadFactory}
import scala.concurrent.{ExecutionContext => SExecutionContext}

package object concurrent {
  type ScheduledExecutionContext = SExecutionContext with ScheduledExecutorService

  object ExecutionContext {
    def fromHandlerThread(thread: HandlerThread): SExecutionContext = {
      val factory = new ThreadFactory {
        def newThread(r: Runnable) = {
          thread
        }
      }

      SExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor(factory))
    }

    object Implicits {
      implicit lazy val scheduledExecutionContext: ScheduledExecutionContext = {
        new ScheduledThreadPoolExecutor(1, Executors.defaultThreadFactory()) with SExecutionContext {
          def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
        }
      }

      implicit val stdExecutionContext: SExecutionContext =
        SExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
    }
  }
}
