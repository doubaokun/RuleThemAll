package sta.tests.benchmarks

import com.artfulbits.benchmark.Meter

abstract class Benchmark {
  val meter: Meter = Meter.getInstance()
  //  meter.setOutput(BenchmarkOutput)

  protected def bench[@specialized A](name: String, reps: Int)(snippet: ⇒ A) = {
    def loop(warmup: Boolean = false) = {
      var i = 0
      var result = snippet
      while (i < reps) {
        result = snippet
        i = i + 1
        if (!warmup) meter.recap()
      }
      result
    }

    meter.start(s"START $name")

    loop(warmup = true)
    meter.skip(s"WARMUP $name")

    meter.loop(reps)
    loop()
    meter.unloop()

    meter.finish(s"END $name")
  }
}
