package kj.android.concurrent

import java.util.concurrent.{FutureTask, ScheduledFuture}
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.Try

trait Task[T] {
  def run(handler: Try[T] => Unit): Unit

  def cancel(mayInterruptIfRunning: Boolean): Boolean
}

object Task {
  def apply[T](body: => T)(implicit ec: ExecutionContext = Implicits.stdExecutionContext): Task[T] =
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
    (implicit ec: ScheduledExecutionContext = Implicits.scheduledExecutionContext): Task[T] =
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
}
