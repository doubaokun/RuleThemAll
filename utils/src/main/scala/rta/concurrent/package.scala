package rta

import java.util.concurrent.{Executors, ScheduledExecutorService, ScheduledThreadPoolExecutor}
import scala.concurrent.{ExecutionContext => SExecutionContext}

package object concurrent {
  type ScheduledExecutionContext = SExecutionContext with ScheduledExecutorService

  object ExecutionContext {
    object Implicits {
      implicit val ec: ScheduledExecutionContext = {
        new ScheduledThreadPoolExecutor(1, Executors.defaultThreadFactory()) with SExecutionContext {
          def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
        }
      }
    }
  }
}
