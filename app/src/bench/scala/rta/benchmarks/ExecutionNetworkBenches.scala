package rta.benchmarks

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import rta.misc.{LoadRules, StateGen}
import rta.model.{BaseModel, ModelKV, Rule}
import rta.parser.RulesParser
import rta.service.ExecutionNetwork
import scala.util.Random
import shapeless.HMap

class ExecutionNetworkBenches extends Bench.Group {
  trait Template extends Bench.OfflineReport with StateGen with LoadRules {
    private[this] val random = new Random(0)

    def typicalRulesGen: Gen[String] = Gen.enumeration("No. rules")(8, 80).map {
      case 8 => "examples.rule"
//      case 80 => "examples_dup10.rule"
    }

    def typicalStateGen: Gen[Vector[BaseModel]] =
      Gen.enumeration("No. entities")(sizes.toSeq.takeWhile(_ <= 100000): _*)
        .map(len => random.shuffle(state(len)).toVector)

    def executionNetworkGen(gen: Gen[String]) = gen.map(f => ExecutionNetwork(loadRulesFromResource(f)))

    performance of "ExecutionNetwork" in {
      measure method "activate" in {
        using(Gen.crossProduct(typicalStateGen, executionNetworkGen(typicalRulesGen))) curve
          "typical" in { case (data, executionNetwork) =>
          val (initial, wmes) = data.splitAt(singleSize)
          var wm = new HMap[ModelKV](Map(initial.map(m => m.companion.Key -> m.lift): _*))
          var result = null.asInstanceOf[TraversableOnce[Rule]]

          wmes.foreach { wme =>
            result = executionNetwork.activate(wme)
            wm.get(wme.companion.Key)(wme.ev) match {
              case Some(seq) => wm = wm.+(wme.companion.Key, wme.mergeTo(seq))(wme.ev)
              case None => wm = wm.+(wme.companion.Key, wme.lift)(wme.ev)
            }
          }

          (result, wm)
        }
      }

      measure method "reference" in {
        using(Gen.crossProduct(typicalStateGen, typicalRulesGen.map(loadRulesFromResource))) curve
          "typical" in { case (data, rules) =>
          val (initial, wmes) = data.splitAt(singleSize)
          var wm = new HMap[ModelKV](Map(initial.map(m => m.companion.Key -> m.lift): _*))
          var result = null.asInstanceOf[TraversableOnce[Rule]]

          wmes.foreach { wme =>
            result = rules.filter(_.satisfiedBy(wm))
            wm.get(wme.companion.Key)(wme.ev) match {
              case Some(seq) => wm = wm.+(wme.companion.Key, wme.mergeTo(seq))(wme.ev)
              case None => wm = wm.+(wme.companion.Key, wme.lift)(wme.ev)
            }
          }

          (result, wm)
        }
    }
  }
  
  performance of "ExecutionNetwork running time" config(
    reports.resultDir -> "target/benchmarks/running_time",
    exec.benchRuns -> 50,
    exec.independentSamples -> 5
  ) in {
    include(new Template {})
  }
}
