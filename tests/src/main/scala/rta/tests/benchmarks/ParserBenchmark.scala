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

    val multiParser = RulesParser.cached(_.Multi)
    val annotatedMulti = RulesParser.cached(_.AnnotatedMulti)

    fromFile(file) { content =>
      val name = file.getName
      val span = name.length + 4 + 3

      builder ++= "|" + ("-" * span) + "|\n"
      builder ++= s"| File $name |\n"
      builder ++= "|" + ("-" * span) + "|\n"

      builder ++= "\n  "
      builder ++= bench("Multi.cached parser", 25) {
        multiParser(content).get
      }

      builder ++= "\n  "
      builder ++= bench("AnnotatedMulti.cached parser", 25) {
        annotatedMulti(content).get
      }

      builder ++= "\n"
    }

    builder.result()
  }
  
  def runPredefined(): String = {
    val builder = new StringBuilder

    val parse = RulesParser.cached(_.AnnotatedMulti)

    builder ++= bench("separate files", 25) {
      val rules = mutable.ListBuffer.empty[Rule]
      assetManager.list("benchmarks/single").foreach(r =>
        fromInputStream(assetManager.open(s"benchmarks/single/$r")) { content =>
          rules += parse(content).get.value.head._1
        }
      )
      rules
    }
    builder += '\n'
//    builder ++= bench("separate files x10", 25) {
//      val rules = mutable.ListBuffer.empty[Rule]
//      assetManager.list("benchmarks/single_dup10").foreach(r =>
//        fromInputStream(assetManager.open(s"benchmarks/single_dup10/$r")) { content =>
//          rules += parse(content).get.value.head._1
//        }
//      )
//      rules
//    }
//    builder += '\n'
//    builder ++= bench("separate files x100", 25) {
//      val rules = mutable.ListBuffer.empty[Rule]
//      assetManager.list("benchmarks/single_dup100").foreach(r =>
//        fromInputStream(assetManager.open(s"benchmarks/single_dup100/$r")) { content =>
//          rules += parse(content).get.value.head._1
//        }
//      )
//      rules
//    }
//    builder += '\n'

    builder ++= bench("single file", 25) {
      fromInputStream(assetManager.open("benchmarks/examples.rule")) { content =>
        parse(content).get.value
      }
    }
    builder += '\n'
//    builder ++= bench("single file x10", 25) {
//      fromInputStream(assetManager.open("benchmarks/examples_dup10.rule")) { content =>
//        parse(content).get.value
//      }
//    }
//    builder += '\n'
//    builder ++= bench("single file x100", 25) {
//      fromInputStream(assetManager.open("benchmarks/examples_dup100.rule")) { content =>
//        parse(content).get.value
//      }
//    }
//    builder += '\n'

    builder.result()
  }
}
