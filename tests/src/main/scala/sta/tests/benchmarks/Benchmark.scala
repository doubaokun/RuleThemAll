package sta.tests.benchmarks

import kj.android.logging.Logging
import org.scalameter.Key._
import org.scalameter._

abstract class Benchmark extends Logging {
  protected def bench[A](name: String, reps: Int)(snippet: => A): Unit = {
    def default() = config(
      preJDK7 -> true,
      exec.maxWarmupRuns -> reps,
      exec.benchRuns -> reps
    ) withWarmer new Warmer.Default

    val time = default() withMeasurer {
      new Measurer.Default
    } measure snippet

    log.info(s"Time for $name: $time")

    val memory = default() withMeasurer {
      new Measurer.MemoryFootprint
    } measure snippet

    log.info(s"Memory for $name: $memory")
  }
}
