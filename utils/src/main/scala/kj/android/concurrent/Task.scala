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
        def run(): Unit = promise.tryComplete(Try(body))
      }, null)

      def cancel(mayInterruptIfRunning: Boolean): Boolean = task.cancel(mayInterruptIfRunning)

      def run(handler: Try[T] => Unit): Unit = {
        ec.execute(task)
        promise.future.onComplete(handler)
      }
    }

  def schedule[T](initialDelay: Duration, period: Duration)(body: => T)
    (implicit ec: ScheduledExecutionContext = Implicits.scheduledExecutionContext): Task[T] =
    new Task[T] {
      @volatile private[this] var started = false

      private[this] val promise = Promise[T]()

      private[this] lazy val scheduled: ScheduledFuture[_] = {
        started = true
        Scheduler.schedule(initialDelay = initialDelay, period = period) {
          promise.complete(Try(body))
        }
      }

      def cancel(mayInterruptIfRunning: Boolean): Boolean = {
        started && scheduled.cancel(mayInterruptIfRunning)
      }

      def run(handler: Try[T] => Unit): Unit = {
        scheduled
        promise.future.onComplete(handler)
      }
    }
}
