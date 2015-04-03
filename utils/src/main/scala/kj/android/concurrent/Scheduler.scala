package kj.android.concurrent

import java.util.concurrent.{ ScheduledExecutorService, TimeUnit }

import scala.concurrent.duration.Duration
import scalaz.\/
import scalaz.concurrent.Future.Async
import scalaz.concurrent.{ Strategy, Task }

object Scheduler {
  def schedule[A](a: ⇒ A, initialDelay: Duration, period: Duration)(implicit pool: ScheduledExecutorService = Strategy.DefaultTimeoutScheduler): Task[A] =
    new Task[A](Async { cb ⇒
      pool.scheduleAtFixedRate(new Runnable {
        def run(): Unit = cb(\/.fromTryCatchNonFatal(a)).run
      }, initialDelay.toMillis, period.toMillis, TimeUnit.MILLISECONDS)
    })
}
