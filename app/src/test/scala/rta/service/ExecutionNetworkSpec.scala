package rta.service

import org.scalacheck._
import org.scalatest.{FlatSpec, Matchers}
import rta.misc.{StateGen, LoadRules}
import rta.model.triggers.Trigger
import rta.model.triggers.Trigger.Branch
import rta.model.triggers.functions.ModelFunction
import rta.model.{ModelKV, ModelHelpers, Rule}
import rta.parser.RulesParser
import rta.tests.PropertyChecks
import shapeless.HMap
import spire.implicits._
import spire.math.UByte

class ExecutionNetworkSpec extends FlatSpec with PropertyChecks with Matchers with StateGen with LoadRules {
  behavior of "Network"

  it should "return same results as reference implementation" in {
    val rules = loadRulesFromResource("multiple.rule").toList
    val network = ExecutionNetwork(rules.iterator)

    sizes.takeWhile(_ < 1000000).map(state(_)).foreach { models =>
      val (initial, wmes) = models.toVector.splitAt(singleSize)
      val initialState = new HMap[ModelKV](initial.map(m => m.companion.Key -> m.lift).toMap[Any, Any])

      network.feed(initial).map(_.name).toList should contain theSameElementsAs rules.filter(_.satisfiedBy(initialState)).map(_.name)
      wmes.foldLeft(initialState) { case (acc, wme) =>
        val newAcc = acc.+(wme.companion.Key -> wme.lift)(wme.ev)
        network.activate(wme).map(_.name).toList should contain theSameElementsAs rules.filter(_.satisfiedBy(newAcc)).map(_.name)
        newAcc
      }
    }
  }
}
