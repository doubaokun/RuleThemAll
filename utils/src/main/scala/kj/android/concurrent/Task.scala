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
  def apply[T](body: => T)(implicit ec: ExecutionContext): Task[T] =
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

  def repeat[T](expression: CronExpression, minWaitTime: Long = 60000)(body: => T)
    (implicit ec: ScheduledExecutionContext): Task[T] =
    new Task[T] {
      @volatile private[this] var scheduled: ScheduledFuture[_] = null

      @inline private def nextDelay(wait: Boolean): Option[Duration] = {
        val from = new Date()
        if (wait) from.setTime(from.getTime + minWaitTime)
        expression.nextDate(from).map(d => (d.getTime - System.currentTimeMillis()).millis)
      }

      private def runRepeatedly(handler: Try[T] => Unit, wait: Boolean): Unit = {
        val promise = Promise[T]()
        nextDelay(wait).foreach(d =>
          scheduled = Scheduler.scheduleOnce(d) {
            promise.tryComplete(Try(body))
            promise.future.onComplete(handler)
          }
        )
        promise.future.onSuccess { case _ => runRepeatedly(handler, wait = true) }
      }

      def cancel(mayInterruptIfRunning: Boolean): Boolean =
        scheduled != null && scheduled.cancel(mayInterruptIfRunning)

      def run(handler: Try[T] => Unit): Unit = runRepeatedly(handler, wait = false)
    }
}
