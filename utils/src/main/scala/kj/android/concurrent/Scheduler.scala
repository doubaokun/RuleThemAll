package kj.android.concurrent

import java.util.concurrent._
import scala.concurrent.duration.Duration

object Scheduler {
  def schedule(initialDelay: Duration, delay: Duration)(body: => Unit)
    (implicit pool: ScheduledExecutorService): ScheduledFuture[_] =
    pool.scheduleWithFixedDelay(new Runnable {
      def run(): Unit = body
    }, initialDelay.toMillis, delay.toMillis, TimeUnit.MILLISECONDS)
}
