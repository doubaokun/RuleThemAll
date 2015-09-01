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

  import Trigger._
  import ops._

  trait Instances {
    val m1 = Trigger.Condition[TestModel](_.i != 1)
    val m2 = Trigger.Condition[TestModel](_.o == TestModel.O.Object1.asInstanceOf[TestModel.O])
    val m3 = Trigger.Condition[TestModel](_.s == "ala ma kota")
    val m4 = Trigger.Condition[TestModel](_.i > 1)
    val m5 = Trigger.Condition[TestModel](_.s != "ala ma kota")
    val m6 = Trigger.Condition[TestModel](_.i < 1)
    val m7 = Trigger.Condition[TestModel](_.o == TestModel.O.Object2.asInstanceOf[TestModel.O])

    def compare(actual: Trigger, expected: Seq[Branch]) =
      actual.flatten.map(_.conditions.toSet) should
        contain theSameElementsAs expected.map(_.conditions.toSet)
  }

  implicit class RichTrigger[M <: Model: ModelCompanion: Uses](trigger: Trigger.Condition[M]) {
    val companion = implicitly[ModelCompanion[M]]
  }

  private def checkTrigger[M <: Model: ModelCompanion: Uses](
    mf: ModelFunction[M], state: HMap[ModelKV]
  )(pred: Boolean => Boolean): Unit = {
    val trigger = Trigger.Condition[M](mf)
    val model: Option[M] = state.get(trigger.companion.Key)(trigger.companion.ev)
    model should be('defined)

    for (m <- model) {
      val p = mf(m)
      whenever(pred(p)) {
        trigger.satisfiedBy(state) should ===(p)
      }
    }
  }

  behavior of "Trigger.flatten"

  it should "have one empty branch on Trigger.empty" in new Instances {
    compare(Trigger.empty, List(Branch()))
  }

  it should "flatten combination of conditions" in new Instances {
    compare(
      actual = Trigger(m1, m2, Trigger.and(m3, Trigger.and(m4, m5, m6))),
      expected = List(Branch(conditions = Seq(m1, m2, m3, m4, m5, m6)))
    )

    compare(
      actual = Trigger(m1, m2, Trigger.or(Trigger.and(m3, m4), Trigger.and(m5, m6))),
      expected = List(
        Branch(conditions = Seq(m1, m2, m3, m4)),
        Branch(conditions = Seq(m1, m2, m5, m6))
      )
    )

    compare(
      actual = Trigger.or(m1, m2, Trigger.and(m3, Trigger.or(m4, m5, m6))),
      expected = List(
        Branch(conditions = Seq(m1)),
        Branch(conditions = Seq(m2)),
        Branch(conditions = Seq(m3, m4)),
        Branch(conditions = Seq(m3, m5)),
        Branch(conditions = Seq(m3, m6))
      )
    )

    compare(
      actual = Trigger(m7, Trigger.or(
        Trigger.and(m4, Trigger.or(m1, Trigger.and(m2, m3))),
        Trigger.and(m5, m6)
      )),
      expected = List(
        Branch(conditions = Seq(m7, m4, m1)),
        Branch(conditions = Seq(m7, m4, m2, m3)),
        Branch(conditions = Seq(m7, m5, m6))
      )
    )
  }

  behavior of "Trigger.Condition"

  it should "yield true if ModelFunction returns true" in {
    forAll(modelFunctionGen[Int], stateGen[Int]) { (mf, state) =>
      checkTrigger(mf, state)(identity)
    }

    forAll(modelFunctionGen[String], stateGen[String]) { (mf, state) =>
      checkTrigger(mf, state)(identity)
    }
  }

  it should "yield false if ModelFunction returns false" in {
    forAll(modelFunctionGen[Int], stateGen[Int]) { (mf, state) =>
      checkTrigger(mf, state)(!_)
    }

    forAll(modelFunctionGen[String], stateGen[String]) { (mf, state) =>
      checkTrigger(mf, state)(!_)
    }
  }

}
