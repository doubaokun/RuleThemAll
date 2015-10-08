package rta.tests.benchmarks

import android.content.res.AssetManager
import java.io.File
import rta.parser.RulesParser

class ParserBenchmark(assetManager: AssetManager) extends Benchmark {
  def withFile(file: File)(f: String => Unit): Unit = {
    f(scala.io.Source.fromFile(file).mkString)
  }
  
  def run(file: File): String = {
    val builder = new StringBuilder

    val multiParser = (str: String) => RulesParser.Multi.parse(str)
    val multiParserCached = RulesParser.cached(_.Multi)
    val annotatedMultiParser = (str: String) => RulesParser.AnnotatedMulti.parse(str)
    val annotatedMultiParserCached = RulesParser.cached(_.AnnotatedMulti)

    withFile(file) { content =>
      val name = file.getName
      val span = name.length + 4 + 3

      builder ++= "|" + ("-" * span) + "|\n"
      builder ++= s"| File $name |\n"
      builder ++= "|" + ("-" * span) + "|\n"

      builder ++= "  "
      builder ++= bench("Multi parser", 10) {
        multiParser(content).get
      }

      builder ++= "\n  "
      builder ++= bench("Multi.cached parser", 10) {
        multiParserCached(content).get
      }

      builder ++= "\n  "
      builder ++= bench("AnnotatedMulti parser", 10) {
        annotatedMultiParser(content).get
      }

      builder ++= "\n  "
      builder ++= bench("AnnotatedMulti.cached parser", 10) {
        annotatedMultiParserCached(content).get
      }

      builder ++= "\n"
    }

    builder.result()
  }
}
