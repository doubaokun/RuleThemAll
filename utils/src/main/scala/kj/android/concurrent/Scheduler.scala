package kj.android.concurrent

import java.util.concurrent._
import scala.concurrent.duration.Duration

object Scheduler {
  def schedule(initialDelay: Duration, period: Duration)(body: => Unit)
    (implicit pool: ScheduledExecutorService): ScheduledFuture[_] =
    pool.scheduleAtFixedRate(new Runnable {
      def run(): Unit = body
    }, initialDelay.toMillis, period.toMillis, TimeUnit.MILLISECONDS)
}
