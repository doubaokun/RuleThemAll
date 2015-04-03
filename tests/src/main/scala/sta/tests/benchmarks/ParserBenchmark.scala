package sta.tests.benchmarks

import android.content.res.AssetManager
import sta.parser.DSLParser

class ParserBenchmark(assetManager: AssetManager) extends Benchmark {
  var content: String = null

  override protected def bench[@specialized A](name: String, reps: Int)(snippet: ⇒ A): Unit = {
    content = scala.io.Source.fromInputStream(assetManager.open(name)).mkString
    super.bench(name, reps)(snippet)
  }

  bench("simple", 100) {
    DSLParser.parse(content)
  }
}
