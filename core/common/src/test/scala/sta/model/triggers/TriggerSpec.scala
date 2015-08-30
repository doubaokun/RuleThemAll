package sta.model.triggers

import org.scalatest.{FlatSpec, Matchers}
import shapeless.HMap
import spire.implicits._
import sta.common.Uses
import sta.common.Uses._
import sta.model.triggers.functions.{NotFunction, ModelFunction}
import sta.model.{Model, ModelCompanion, ModelHelpers, ModelKV}
import sta.tests.PropertyChecks

class TriggerSpec extends FlatSpec with PropertyChecks with Matchers with ModelHelpers {

  import Condition._
  import ops._

  trait Instances {
    val m1 = Condition.Trigger[TestModel](_.i != 1)
    val notM1 = Condition.Trigger[TestModel](NotFunction(m1.function))
    val m2 = Condition.Trigger[TestModel](_.o == TestModel.O.Object1.asInstanceOf[TestModel.O])
    val notM2 = Condition.Trigger[TestModel](NotFunction(m2.function))
    val m3 = Condition.Trigger[TestModel](_.s == "ala ma kota")
    val notM3 = Condition.Trigger[TestModel](NotFunction(m3.function))
    val m4 = Condition.Trigger[TestModel](_.i > 1)
    val m5 = Condition.Trigger[TestModel](_.s != "ala ma kota")
    val m6 = Condition.Trigger[TestModel](_.i < 1)
    val m7 = Condition.Trigger[TestModel](_.o == TestModel.O.Object2.asInstanceOf[TestModel.O])

    def compare(actual: Condition, expected: Seq[Branch]) =
      actual.flatten.map(_.triggers.toSet) should contain theSameElementsAs expected.map(_.triggers.toSet)
  }

  implicit class RichTrigger[M <: Model: ModelCompanion: Uses](trigger: Condition.Trigger[M]) {
    val companion = implicitly[ModelCompanion[M]]
  }

  private def checkAtomicTrigger[M <: Model: ModelCompanion: Uses](
    mf: ModelFunction[M], state: HMap[ModelKV]
  )(pred: Boolean => Boolean): Unit = {
    val trigger = Condition.Trigger[M](mf)
    val model: Option[M] = state.get(trigger.companion.Key)(trigger.companion.ev)
    model should be('defined)

    for (m <- model) {
      val p = mf(m)
      whenever(pred(p)) {
        trigger.satisfiedBy(state) should ===(p)
      }
    }
  }

  behavior of "Condition.flatten"

  it should "have one empty branch on Condition.empty" in new Instances {
    compare(Condition.empty, List(Branch()))
  }

  it should "flatten combination of conditions" in new Instances {
    compare(
      actual = Condition(m1, m2, Condition.and(m3, Condition.and(m4, m5, m6))),
      expected = List(Branch(triggers = Seq(m1, m2, m3, m4, m5, m6)))
    )

    compare(
      actual = Condition(m1, m2, Condition.or(Condition.and(m3, m4), Condition.and(m5, m6))),
      expected = List(
        Branch(triggers = Seq(m1, m2, m3, m4)),
        Branch(triggers = Seq(m1, m2, m5, m6))
      )
    )

    compare(
      actual = Condition.or(m1, m2, Condition.and(m3, Condition.or(m4, m5, m6))),
      expected = List(
        Branch(triggers = Seq(m1)),
        Branch(triggers = Seq(m2)),
        Branch(triggers = Seq(m3, m4)),
        Branch(triggers = Seq(m3, m5)),
        Branch(triggers = Seq(m3, m6))
      )
    )

    compare(
      actual = Condition(m7, Condition.or(Condition.and(m4, Condition.xor(m1, Condition.and(m2, m3))), Condition.or(m5, m6))),
      expected = List(
        Branch(triggers = Seq(m7, m4, m1, notM2, notM3)),
        Branch(triggers = Seq(m7, m4, notM1, m2, m3)),
        Branch(triggers = Seq(m7, m5)),
        Branch(triggers = Seq(m7, m6))
      )
    )
  }

  behavior of "Condition.Trigger"

  it should "yield true if ModelFunction returns true" in {
    forAll(modelFunctionGen[Int], stateGen[Int]) { (mf, state) =>
      checkAtomicTrigger(mf, state)(identity)
    }

    forAll(modelFunctionGen[String], stateGen[String]) { (mf, state) =>
      checkAtomicTrigger(mf, state)(identity)
    }
  }

  it should "yield false if ModelFunction returns false" in {
    forAll(modelFunctionGen[Int], stateGen[Int]) { (mf, state) =>
      checkAtomicTrigger(mf, state)(!_)
    }

    forAll(modelFunctionGen[String], stateGen[String]) { (mf, state) =>
      checkAtomicTrigger(mf, state)(!_)
    }
  }

}
