package kj.android.concurrent

import java.util.concurrent.{FutureTask, ScheduledFuture}
import scala.concurrent.Promise
import scala.concurrent.duration.Duration
import scala.util.Try

trait Task[T] {
  def run(handler: Try[T] => Unit): Unit

  def cancel(mayInterruptIfRunning: Boolean): Boolean
}

object Task {
  import scala.concurrent.ExecutionContext.Implicits.global

  def apply[T](body: => T): Task[T] = new Task[T] {
    private val promise = Promise[T]()

    private val task = new FutureTask[Unit](new Runnable {
      def run(): Unit = promise.tryComplete(Try(body))
    }, null)

    def cancel(mayInterruptIfRunning: Boolean): Boolean = task.cancel(mayInterruptIfRunning)

    def run(handler: Try[T] => Unit): Unit = {
      task.run()
      promise.future.onComplete(handler)
    }
  }

  def schedule[T](initialDelay: Duration, period: Duration)(body: => T): Task[T] = new Task[T] {
    @volatile private var started = false

    private val promise = Promise[T]()

    private lazy val scheduled: ScheduledFuture[_] = {
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
