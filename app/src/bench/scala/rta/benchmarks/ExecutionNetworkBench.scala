package rta.benchmarks

import rta.model.{BaseModel, Rule}
import rta.service.ExecutionNetwork

trait ExecutionNetworkBench extends Bench.LocalTime with StateGen {
  def rulesGen: Gen[Iterator[Rule]]
  
  lazy val activateGen: Gen[(ExecutionNetwork, Iterator[BaseModel])] =
    Gen.crossProduct(rulesGen.map(ExecutionNetwork(_)), stateGen)

//  lazy val referenceGen: Gen[(Iterator[Rule], Iterator[HMap[ModelKV]])] =
//    Gen.crossProduct(rulesGen, stateGen.map(ms => ms.map(m => HMap.empty(Map(m.companion.Key -> m)))))
  
  performance of "ExecutionNetwork" in {
    measure method "compile" in {
      using(rulesGen) in { r => ExecutionNetwork(r) }
    }

    measure method "activate" in {
      using(activateGen) in { case (executionNetwork, wmes) =>
        wmes.map(executionNetwork.activate)
      }
    }
    
    measure method "reference" in {
//      using(referenceGen) in { case (rules, wmes) =>
//        wmes.reduceLeft[HMap[ModelKV]] { case (acc, e) => })
//        rules.map(_.satisfiedBy(wmes))
//      }
    }
  }
}

class ExecutionNetworkBenches(rules: Gen[Iterator[Rule]]) extends Bench.Group {
  performance of "ExecutionNetwork running time" in {
    include(new ExecutionNetworkBench {
      def rulesGen = rules
    })
  }
}
