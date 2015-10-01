package sta.tests.benchmarks

import android.content.res.AssetManager
import sta.parser.RulesParser

class ParserBenchmark(assetManager: AssetManager) extends Benchmark {
  def withFile(filename: String)(f: String => Unit): Unit = {
    f(scala.io.Source.fromInputStream(assetManager.open(s"benchmarks/$filename")).mkString)
  }
  
  def run(): String = {
    val builder = new StringBuilder

    val singleParser = RulesParser.cached(_.Single).andThen(_.get)
    val multiParser = RulesParser.cached(_.Multi).andThen(_.get)
    val annotatedMultiParser = RulesParser.cached(_.AnnotatedMulti).andThen(_.get)

    withFile("single.rule") { content =>
      builder ++= "|------------------|\n"
      builder ++= "| File single.rule |\n"
      builder ++= "|------------------|\n"

      builder ++= "  "
      builder ++= bench("Single parser", 100) {
        singleParser(content)
      }

      builder ++= "\n  "
      builder ++= bench("Multi parser", 100) {
        multiParser(content)
      }

      builder ++= "\n  "
      builder ++= bench("AnnotatedMulti parser", 100) {
        annotatedMultiParser(content)
      }
      builder ++= "\n"
    }

    withFile("multiple.rule") { content =>
      builder ++= "|--------------------|\n"
      builder ++= "| File multiple.rule |\n"
      builder ++= "|--------------------|\n"

      builder ++= "  "
      builder ++= bench("Multi parser", 100) {
        multiParser(content)
      }

      builder ++= "\n  "
      builder ++= bench("AnnotatedMulti parser", 100) {
        annotatedMultiParser(content)
      }
      builder ++= "\n"
    }

    builder.result()
  }
}
