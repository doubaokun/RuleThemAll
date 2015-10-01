package sta.tests.benchmarks

import org.scalameter.Key._
import org.scalameter._
import sta.logging.Logging

abstract class Benchmark extends Logging {
  protected def bench[A](name: String, reps: Int)(snippet: => A): String = {
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

    f"""$name
       |    Time: ${time.value}%.2f [${time.units}]
       |    Memory: ${memory.value}%.2f [${memory.units}]
    """.stripMargin
  }
}
