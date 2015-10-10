package rta.tests.benchmarks

import android.content.res.AssetManager
import java.io.{InputStream, File}
import rta.model.Rule
import rta.parser.RulesParser
import scala.collection.mutable

class ParserBenchmark(assetManager: AssetManager) extends Benchmark {
  def fromFile[T](file: File)(f: String => T): T = {
    f(scala.io.Source.fromFile(file).mkString)
  }
  
  def fromInputStream[T](is: InputStream)(f: String => T): T  = {
    f(scala.io.Source.fromInputStream(is).mkString)
  }
  
  def runCustom(file: File): String = {
    val builder = new StringBuilder

    val multiParser = (str: String) => RulesParser.Multi.parse(str)
    val multiParserCached = RulesParser.cached(_.Multi)
    val annotatedMultiParser = (str: String) => RulesParser.AnnotatedMulti.parse(str)
    val annotatedMultiParserCached = RulesParser.cached(_.AnnotatedMulti)

    fromFile(file) { content =>
      val name = file.getName
      val span = name.length + 4 + 3

      builder ++= "|" + ("-" * span) + "|\n"
      builder ++= s"| File $name |\n"
      builder ++= "|" + ("-" * span) + "|\n"

      builder ++= "  "
      builder ++= bench("Multi parser", 25) {
        multiParser(content).get
      }

      builder ++= "\n  "
      builder ++= bench("Multi.cached parser", 25) {
        multiParserCached(content).get
      }

      builder ++= "\n  "
      builder ++= bench("AnnotatedMulti parser", 25) {
        annotatedMultiParser(content).get
      }

      builder ++= "\n  "
      builder ++= bench("AnnotatedMulti.cached parser", 25) {
        annotatedMultiParserCached(content).get
      }

      builder ++= "\n"
    }

    builder.result()
  }
  
  def runPredefined(): String = {
    val builder = new StringBuilder

    val parser = RulesParser.cached(_.AnnotatedMulti)

    builder ++= "|-------------------|\n"
    builder ++= "| Parser allocation |\n"
    builder ++= "|-------------------|\n"
    builder ++= bench("parser allocation", 25) {
       RulesParser.cached(_.AnnotatedMulti)
    }

    builder ++= "|-----------------------|\n"
    builder ++= "| All in separate files |\n"
    builder ++= "|-----------------------|\n"
    builder ++= bench("separate files", 25) {
      val rules = mutable.ListBuffer.empty[Rule]

      assetManager.list("benchmarks/single").foreach(r =>
        fromInputStream(assetManager.open(s"benchmarks/single/$r")) { content =>
          rules += parser(content).get.value.head._1
        }
      )

      rules
    }

    builder ++= "|-----------------|\n"
    builder ++= "| All in single file |\n"
    builder ++= "|-----------------|\n"
    builder ++= bench("single file", 25) {
      fromInputStream(assetManager.open("benchmarks/all.rule")) { content =>
        RulesParser.cached(_.AnnotatedMulti)(content).get.value
      }
    }

    builder.result()
  }
}
