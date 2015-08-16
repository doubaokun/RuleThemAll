package sta.model.triggers

import org.scalatest.{Matchers, WordSpec}
import shapeless.HMap
import spire.implicits._
import sta.common.UsedFeatures
import sta.common.UsedFeatures._
import sta.model.triggers.functions.ModelFunction
import sta.model.{Model, ModelCompanion, ModelHelpers, ModelKV}
import sta.tests.PropertyChecks

class TriggerSpec extends WordSpec with PropertyChecks with Matchers with ModelHelpers {

  import ops._

  implicit class RichTrigger[M <: Model: ModelCompanion: UsedFeatures](trigger: AtomicTrigger[M]) {
    val companion = implicitly[ModelCompanion[M]]
  }

  private def checkAtomicTrigger[M <: Model: ModelCompanion: UsedFeatures](
    mf: ModelFunction[M], state: HMap[ModelKV]
  )(pred: Boolean => Boolean): Unit = {
    val trigger = AtomicTrigger[M](mf)
    val model: Option[M] = state.get(trigger.companion.Key)(trigger.companion.ev)
    model should be('defined)

    for (m <- model) {
      val p = mf(m)
      whenever(pred(p)) {
        trigger.satisfiedBy(state) should ===(p)
      }
    }
  }

  private def checkLogicOpTrigger[M1 <: Model: ModelCompanion: UsedFeatures, M2 <: Model: ModelCompanion: UsedFeatures](
    mf1: ModelFunction[M1], state1: HMap[ModelKV],
    mf2: ModelFunction[M2], state2: HMap[ModelKV]
  )(
    f:     (Trigger, Trigger) => LogicOpTrigger,
    tPred: (Boolean, Boolean) => Boolean, pred: Boolean => Boolean
  ): Unit = {
    val t1 = AtomicTrigger[M1](mf1)
    val t2 = AtomicTrigger[M2](mf2)
    val model1 = state1.get(t1.companion.Key)(t1.companion.ev)
    val model2 = state2.get(t2.companion.Key)(t2.companion.ev)

    model1 should be('defined)
    model2 should be('defined)

    for {
      m1 <- model1
      m2 <- model2
    } {
      val state = HMap[ModelKV](t1.companion.Key -> m1, t2.companion.Key -> m2)(t1.companion.ev, t2.companion.ev)
      val trigger = f(t1, t2)
      val p = tPred(t1.satisfiedBy(state), t2.satisfiedBy(state))
      whenever(pred(p)) {
        trigger.satisfiedBy(state) should ===(p)
      }
    }
  }

  "An AtomicTrigger" should {
    "yield true if ModelFunction returns true" in {
      forAll(modelFunctionGen[Int], stateGen[Int]) { (mf, state) =>
        checkAtomicTrigger(mf, state)(identity)
      }

      forAll(modelFunctionGen[String], stateGen[String]) { (mf, state) =>
        checkAtomicTrigger(mf, state)(identity)
      }
    }

    "yield false if ModelFunction returns false" in {
      forAll(modelFunctionGen[Int], stateGen[Int]) { (mf, state) =>
        checkAtomicTrigger(mf, state)(!_)
      }

      forAll(modelFunctionGen[String], stateGen[String]) { (mf, state) =>
        checkAtomicTrigger(mf, state)(!_)
      }
    }
  }

  "A LogicOpTrigger" when {
    "is an AndTrigger" should {
      "yield true if both triggers are satisfied" in {
        forAll(modelFunctionGen[String], stateGen[String],
          modelFunctionGen[Int], stateGen[Int]) { (mf1, state1, mf2, state2) =>
          checkLogicOpTrigger(mf1, state1, mf2, state2)(AndTrigger, _ && _, identity)
        }
      }

      "yield false if not both triggers are satisfied" in {
        forAll(modelFunctionGen[String], stateGen[String],
          modelFunctionGen[Int], stateGen[Int]) { (mf1, state1, mf2, state2) =>
          checkLogicOpTrigger(mf1, state1, mf2, state2)(AndTrigger, _ && _, !_)
        }
      }
    }

    "is an OrTrigger" should {
      "yield true if at least one of triggers is satisfied" in {
        forAll(modelFunctionGen[String], stateGen[String],
          modelFunctionGen[Int], stateGen[Int]) { (mf1, state1, mf2, state2) =>
          checkLogicOpTrigger(mf1, state1, mf2, state2)(OrTrigger, _ || _, identity)
        }
      }

      "yield false if none trigger is satisfied" in {
        forAll(modelFunctionGen[String], stateGen[String],
          modelFunctionGen[Int], stateGen[Int]) { (mf1, state1, mf2, state2) =>
          checkLogicOpTrigger(mf1, state1, mf2, state2)(OrTrigger, _ || _, !_)
        }
      }
    }

    "is a XorTrigger" should {
      "yield true if only one of triggers is satisfied" in {
        forAll(modelFunctionGen[String], stateGen[String],
          modelFunctionGen[Int], stateGen[Int]) { (mf1, state1, mf2, state2) =>
          checkLogicOpTrigger(mf1, state1, mf2, state2)(XorTrigger, (l, r) => (l && !r) || (!l && r), identity)
        }
      }

      "yield false if both triggers are satisfied or none trigger is satisfied" in {
        forAll(modelFunctionGen[String], stateGen[String],
          modelFunctionGen[Int], stateGen[Int]) { (mf1, state1, mf2, state2) =>
          checkLogicOpTrigger(mf1, state1, mf2, state2)(XorTrigger, (l, r) => (l && !r) || (!l && r), !_)
        }
      }
    }
  }
}
