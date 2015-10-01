package sta.concurrent

import java.util.concurrent._
import scala.concurrent.duration.Duration

object Scheduler {
  def schedule(initialDelay: Duration, delay: Duration)(body: => Unit)
    (implicit pool: ScheduledExecutorService): ScheduledFuture[_] =
    pool.scheduleWithFixedDelay(new Runnable {
      def run(): Unit = body
    }, initialDelay.toMillis, delay.toMillis, TimeUnit.MILLISECONDS)

  def scheduleOnce[T](delay: Duration)(body: => T)
    (implicit pool: ScheduledExecutorService): ScheduledFuture[T] =
    pool.schedule(new Callable[T] {
      def call(): T = body
    }, delay.toMillis, TimeUnit.MILLISECONDS)
}
