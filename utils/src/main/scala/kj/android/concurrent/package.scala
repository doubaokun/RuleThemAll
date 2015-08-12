package kj.android

import java.util.concurrent.{ScheduledThreadPoolExecutor, ScheduledExecutorService, Executors, ThreadFactory}
import scala.concurrent.ExecutionContext

package object concurrent {
  type ScheduledExecutionContext = ExecutionContext with ScheduledExecutorService

  object Implicits {
    implicit lazy val scheduledExecutionContext: ScheduledExecutionContext = {
      val factory = new ThreadFactory {
        val defaultThreadFactory = Executors.defaultThreadFactory()

        def newThread(r: Runnable) = {
          val t = defaultThreadFactory.newThread(r)
          t.setDaemon(true)
          t
        }
      }

      new ScheduledThreadPoolExecutor(1, factory) with ExecutionContext {
        def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
      }
    }

    implicit val stdExecutionContext: ExecutionContext =
      scala.concurrent.ExecutionContext.Implicits.global
  }
}
