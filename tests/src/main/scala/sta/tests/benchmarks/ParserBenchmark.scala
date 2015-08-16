package sta.tests.benchmarks

import android.content.res.AssetManager
import sta.parser.RulesParser

class ParserBenchmark(assetManager: AssetManager) extends Benchmark {
  var content: String = null

  override protected def bench[A](name: String, reps: Int)(snippet: => A): Unit = {
    content = scala.io.Source.fromInputStream(assetManager.open(s"benchmarks/$name")).mkString
    super.bench(name, reps)(snippet)
  }

  bench("simple", 50) {
    RulesParser.parse(content)
  }
}
