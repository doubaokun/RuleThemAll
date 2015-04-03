package sta.model.triggers

import kj.android.common.UsedFeatures
import org.scalatest.{ Matchers, WordSpec }
import shapeless.HMap
import sta.model.{ ModelKV, ModelCompanion, Model, ModelHelpers }
import sta.model.triggers.functions.{ EqualFunction, ModelFunction }
import kj.android.common.UsedFeatures._
import sta.tests.PropertyChecks

import scalaz.Scalaz.{ ToEqualOps ⇒ _ }
import spire.implicits._

class TriggerSpec extends WordSpec with PropertyChecks with Matchers with ModelHelpers {

  import ops._

  implicit class RichTrigger[M <: Model: ModelCompanion: UsedFeatures](trigger: AtomicTrigger[M]) {
    val companion = implicitly[ModelCompanion[M]]
  }

  private def checkAtomicTrigger[M <: Model: ModelCompanion: UsedFeatures](mf: ModelFunction[M],
                                                                           state: HMap[ModelKV])(pred: Boolean ⇒ Boolean) {
    val trigger = AtomicTrigger[M](mf)
    val model: Option[M] = state.get(trigger.companion.Key)(trigger.companion.ev)
    model should be('defined)

    val p = mf(model.get)
    whenever(pred(p)) { trigger.satisfiedBy(state) should be === p }
  }

  private def checkLogicOpTrigger[M1 <: Model: ModelCompanion: UsedFeatures, M2 <: Model: ModelCompanion: UsedFeatures](
    mf1: ModelFunction[M1], state1: HMap[ModelKV], mf2: ModelFunction[M2], state2: HMap[ModelKV])(
      f: (Trigger, Trigger) ⇒ LogicOpTrigger, tPred: (Boolean, Boolean) ⇒ Boolean, pred: Boolean ⇒ Boolean) {
    val t1 = AtomicTrigger[M1](mf1)
    val t2 = AtomicTrigger[M2](mf2)
    val m1 = state1.get(t1.companion.Key)(t1.companion.ev)
    val m2 = state2.get(t2.companion.Key)(t2.companion.ev)

    m1 should be('defined)
    m2 should be('defined)

    val state = HMap[ModelKV](t1.companion.Key -> m1.get, t2.companion.Key -> m2.get)(t1.companion.ev, t2.companion.ev)
    val trigger = f(t1, t2)
    val p = tPred(t1.satisfiedBy(state), t2.satisfiedBy(state))
    whenever(pred(p)) { trigger.satisfiedBy(state) should be === p }
  }

  "An AtomicTrigger" when {
    "using wrapped primitive" should {
      "yield true if ModelFunction returns true" in {
        forAll(modelFunctionGen[Int, IntWrapper], stateGen[Int, IntWrapper]) { (mf, state) ⇒
          checkAtomicTrigger(mf, state)(identity)
        }
      }

      "yield false if ModelFunction returns false" in {
        forAll(modelFunctionGen[Int, IntWrapper], stateGen[Int, IntWrapper]) { (mf, state) ⇒
          checkAtomicTrigger(mf, state)(!_)
        }
      }
    }

    "using wrapped reference" should {
      "yield true if ModelFunction returns true" in {
        forAll(modelFunctionGen[String, StringWrapper], stateGen[String, StringWrapper]) { (mf, state) ⇒
          checkAtomicTrigger(mf, state)(identity)
        }
      }

      "yield false if ModelFunction returns false" in {
        forAll(modelFunctionGen[String, StringWrapper], stateGen[String, StringWrapper]) { (mf, state) ⇒
          checkAtomicTrigger(mf, state)(!_)
        }
      }
    }
  }

  "A LogicOpTrigger" when {
    "is an AndTrigger" should {
      "yield true if both triggers are satisfied" in {
        forAll(modelFunctionGen[String, StringWrapper], stateGen[String, StringWrapper],
          modelFunctionGen[Int, IntWrapper], stateGen[Int, IntWrapper]) { (mf1, state1, mf2, state2) ⇒
            checkLogicOpTrigger(mf1, state1, mf2, state2)(AndTrigger, _ && _, identity)
          }
      }

      "yield false if not both triggers are satisfied" in {
        forAll(modelFunctionGen[String, StringWrapper], stateGen[String, StringWrapper],
          modelFunctionGen[Int, IntWrapper], stateGen[Int, IntWrapper]) { (mf1, state1, mf2, state2) ⇒
            checkLogicOpTrigger(mf1, state1, mf2, state2)(AndTrigger, _ && _, !_)
          }
      }
    }

    "is an OrTrigger" should {
      "yield true if at least one of triggers is satisfied" in {
        forAll(modelFunctionGen[String, StringWrapper], stateGen[String, StringWrapper],
          modelFunctionGen[Int, IntWrapper], stateGen[Int, IntWrapper]) { (mf1, state1, mf2, state2) ⇒
            checkLogicOpTrigger(mf1, state1, mf2, state2)(OrTrigger, _ || _, identity)
          }
      }

      "yield false if none trigger is satisfied" in {
        forAll(modelFunctionGen[String, StringWrapper], stateGen[String, StringWrapper],
          modelFunctionGen[Int, IntWrapper], stateGen[Int, IntWrapper]) { (mf1, state1, mf2, state2) ⇒
            checkLogicOpTrigger(mf1, state1, mf2, state2)(OrTrigger, _ || _, !_)
          }
      }
    }

    "is a XorTrigger" should {
      "yield true if only one of triggers is satisfied" in {
        forAll(modelFunctionGen[String, StringWrapper], stateGen[String, StringWrapper],
          modelFunctionGen[Int, IntWrapper], stateGen[Int, IntWrapper]) { (mf1, state1, mf2, state2) ⇒
            checkLogicOpTrigger(mf1, state1, mf2, state2)(XorTrigger, (l, r) ⇒ (l && !r) || (!l && r), identity)
          }
      }

      "yield false if both triggers are satisfied or none trigger is satisfied" in {
        forAll(modelFunctionGen[String, StringWrapper], stateGen[String, StringWrapper],
          modelFunctionGen[Int, IntWrapper], stateGen[Int, IntWrapper]) { (mf1, state1, mf2, state2) ⇒
            checkLogicOpTrigger(mf1, state1, mf2, state2)(XorTrigger, (l, r) ⇒ (l && !r) || (!l && r), !_)
          }
      }
    }
  }
}
