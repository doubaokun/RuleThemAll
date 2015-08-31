package kj.android.concurrent

import java.util.Date
import java.util.concurrent.{FutureTask, ScheduledFuture}
import kj.android.cron.CronExpression
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.Try

trait Task[T] {
  def run(handler: Try[T] => Unit): Unit

  def cancel(mayInterruptIfRunning: Boolean): Boolean
}

object Task {
  def join[T](tasks: Seq[Task[T]]): Task[T] = new Task[T] {
    def cancel(mayInterruptIfRunning: Boolean): Boolean = tasks.forall(_.cancel(mayInterruptIfRunning))

    def run(handler: Try[T] => Unit): Unit = tasks.foreach(_.run(handler))
  }

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

  def scheduleOnce[T](delay: Duration)(body: => T)
    (implicit ec: ScheduledExecutionContext = Implicits.scheduledExecutionContext): Task[T] =
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

  def repeat[T](expression: CronExpression)(body: => T)
    (implicit ec: ScheduledExecutionContext = Implicits.scheduledExecutionContext): Task[T] =
    new Task[T] {
      @volatile private[this] var scheduled: ScheduledFuture[_] = null

      @inline private def nextDelay: Option[Duration] =
        expression.nextDate(new Date).map(d => (d.getTime - System.currentTimeMillis()).millis)

      def cancel(mayInterruptIfRunning: Boolean): Boolean =
        scheduled != null && scheduled.cancel(mayInterruptIfRunning)

      def run(handler: Try[T] => Unit): Unit = {
        val promise = Promise[T]()
        nextDelay.foreach(d =>
          scheduled = Scheduler.scheduleOnce(d) {
            promise.tryComplete(Try(body))
            promise.future.onComplete(handler)
          }
        )
        promise.future.onSuccess { case _ => run(handler) }
      }
    }
}
