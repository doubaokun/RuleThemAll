package sta.tests.benchmarks

import android.content.res.AssetManager
import org.scalameter.Quantity
import sta.parser.RulesParser

class ParserBenchmark(assetManager: AssetManager) extends Benchmark {
  def withFile(filename: String)(f: String => Unit): Unit = {
    f(scala.io.Source.fromInputStream(assetManager.open(s"benchmarks/$filename")).mkString)
  }
  
  def run(): String = {
    val builder = new StringBuilder
    
    withFile("single.rule") { content =>
      builder ++= bench("single", 10) {
        RulesParser.parse(content)
      }
      builder += '\n'

      builder ++= bench("single.single", 10) {
        RulesParser.parseSingle(content)
      }
      builder += '\n'

      builder ++= bench("single.traced", 10) {
        RulesParser.tracedParse(content)
      }
      builder += '\n'
    }

    withFile("multiple.rule") { content =>
      builder ++= bench("multiple", 10) {
        RulesParser.parse(content)
      }
      builder += '\n'

      builder ++= bench("multiple.traced", 10) {
        RulesParser.tracedParse(content)
      }
      builder += '\n'
    }

    builder.result()
  }
}
