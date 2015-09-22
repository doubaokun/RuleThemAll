package sta.tests.benchmarks

import android.content.res.AssetManager
import sta.parser.RulesParser

class ParserBenchmark(assetManager: AssetManager) extends Benchmark {
  def withFile(filename: String)(f: String => Unit): Unit = {
    f(scala.io.Source.fromInputStream(assetManager.open(s"benchmarks/$filename")).mkString)
  }

  withFile("single.rule") { content =>
    bench("single", 50) {
      RulesParser.parse(content)
    }

    bench("single.single", 50) {
      RulesParser.parseSingle(content)
    }

    bench("single.traced", 50) {
      RulesParser.tracedParse(content)
    }
  }

  withFile("multiple.rule") { content =>
    bench("multiple", 50) {
      RulesParser.parse(content)
    }

    bench("multiple.traced", 50) {
      RulesParser.tracedParse(content)
    }
  }
}
