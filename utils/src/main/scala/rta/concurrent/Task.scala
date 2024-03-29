package rta.concurrent

import android.content.Context
import android.os.PowerManager
import java.util.concurrent.{FutureTask, ScheduledFuture}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext => SExecutionContext, Promise}
import scala.util.Try
import rta.common.SystemServices._
import rta.logging.LogTag

trait Task[T] {
  def run(handler: Try[T] => Unit): Unit

  def cancel(mayInterruptIfRunning: Boolean): Boolean
}

object Task {
  def runWithWakeLock[T](body: => T)(implicit ctx: Context, logTag: LogTag, ec: SExecutionContext): Unit = {
    val lock = powerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, logTag.tag)
    apply {
      lock.acquire()
      body
    }.run(_ => lock.release())
  }
  
  def apply[T](body: => T)(implicit ec: SExecutionContext): Task[T] =
    new Task[T] {
      private[this] val promise = Promise[T]()

      private[this] val task = new FutureTask[Unit](new Runnable {
        def run(): Unit = {
          promise.tryComplete(Try(body))
        }
      }, null)

      def cancel(mayInterruptIfRunning: Boolean): Boolean = task.cancel(mayInterruptIfRunning)

      def run(handler: Try[T] => Unit): Unit = {
        ec.execute(task)
        promise.future.onComplete(handler)
      }
    }

  def schedule[T](initialDelay: Duration, delay: Duration)(body: => T)
    (implicit ec: ScheduledExecutionContext): Task[T] =
    new Task[T] {
      @volatile private[this] var scheduled: ScheduledFuture[_] = null

      def cancel(mayInterruptIfRunning: Boolean): Boolean = {
        (scheduled != null) && {
          val result = scheduled.cancel(mayInterruptIfRunning)
          scheduled = null
          result
        }
      }

      def run(handler: Try[T] => Unit): Unit = {
        scheduled = Scheduler.schedule(initialDelay = initialDelay, delay = delay) {
          val promise = Promise[T]()
          promise.tryComplete(Try(body))
          promise.future.onComplete(handler)
        }
      }
    }

  def scheduleOnce[T](delay: Duration)(body: => T)
    (implicit ec: ScheduledExecutionContext): Task[T] =
    new Task[T] {
      @volatile private[this] var scheduled: ScheduledFuture[_] = null

      def cancel(mayInterruptIfRunning: Boolean): Boolean =
        scheduled != null && scheduled.cancel(mayInterruptIfRunning)

      def run(handler: Try[T] => Unit): Unit = {
        scheduled = Scheduler.scheduleOnce(delay) {
          val promise = Promise[T]()
          promise.tryComplete(Try(body))
          promise.future.onComplete(handler)
        }
      }
    }
}
