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
    val m1 = Trigger.Atomic[TestModel](_.i != 1)
    val notM1 = Trigger.Atomic[TestModel](NotFunction(m1.function))
    val m2 = Trigger.Atomic[TestModel](_.o == TestModel.O.Object1.asInstanceOf[TestModel.O])
    val notM2 = Trigger.Atomic[TestModel](NotFunction(m2.function))
    val m3 = Trigger.Atomic[TestModel](_.s == "ala ma kota")
    val notM3 = Trigger.Atomic[TestModel](NotFunction(m3.function))
    val m4 = Trigger.Atomic[TestModel](_.i > 1)
    val m5 = Trigger.Atomic[TestModel](_.s != "ala ma kota")
    val m6 = Trigger.Atomic[TestModel](_.i < 1)
    val m7 = Trigger.Atomic[TestModel](_.o == TestModel.O.Object2.asInstanceOf[TestModel.O])

    def compare(actual: Trigger, expected: Seq[Branch]) =
      actual.flatten.map(_.triggers.toSet) should contain theSameElementsAs expected.map(_.triggers.toSet)
  }

  implicit class RichTrigger[M <: Model: ModelCompanion: Uses](trigger: Trigger.Atomic[M]) {
    val companion = implicitly[ModelCompanion[M]]
  }

  private def checkAtomicTrigger[M <: Model: ModelCompanion: Uses](
    mf: ModelFunction[M], state: HMap[ModelKV]
  )(pred: Boolean => Boolean): Unit = {
    val trigger = Trigger.Atomic[M](mf)
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
    compare(Trigger.empty, List(Branch(Nil)))
  }

  it should "flatten combination of triggers" in new Instances {
    compare(
      actual = Trigger(m1, m2, Trigger.and(m3, Trigger.and(m4, m5, m6))),
      expected = List(Branch(Seq(m1, m2, m3, m4, m5, m6)))
    )

    compare(
      actual = Trigger(m1, m2, Trigger.or(Trigger.and(m3, m4), Trigger.and(m5, m6))),
      expected = List(
        Branch(Seq(m1, m2, m3, m4)),
        Branch(Seq(m1, m2, m5, m6))
      )
    )

    compare(
      actual = Trigger.or(m1, m2, Trigger.and(m3, Trigger.or(m4, m5, m6))),
      expected = List(
        Branch(Seq(m1)),
        Branch(Seq(m2)),
        Branch(Seq(m3, m4)),
        Branch(Seq(m3, m5)),
        Branch(Seq(m3, m6))
      )
    )

    compare(
      actual = Trigger(m7, Trigger.or(Trigger.and(m4, Trigger.xor(m1, Trigger.and(m2, m3))), Trigger.or(m5, m6))),
      expected = List(
        Branch(Seq(m7, m4, m1, notM2, notM3)),
        Branch(Seq(m7, m4, notM1, m2, m3)),
        Branch(Seq(m7, m5)),
        Branch(Seq(m7, m6))
      )
    )
  }

  behavior of "Trigger.Atomic"

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
