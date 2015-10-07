package rta.model.triggers

import org.scalatest.{FlatSpec, Matchers}
import rta.cron.CronExpression
import scala.concurrent.duration.Duration
import shapeless.HMap
import spire.implicits._
import rta.common.Uses
import rta.common.Uses._
import rta.model.triggers.functions.ModelFunction
import rta.model.{BaseModel, BaseModelCompanion, ModelHelpers, ModelKV}
import rta.tests.PropertyChecks

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

  private def checkTrigger[M <: BaseModel: BaseModelCompanion: Uses](
    mf: ModelFunction[M], state: HMap[ModelKV]
  )(pred: Boolean => Boolean): Unit = {
    val companion = implicitly[BaseModelCompanion[M]]
    import companion._

    val trigger = Trigger.Condition[M](mf)
    val model = state.get(Key)
    model should be('defined)

    for (m <- model) {
      val p = exists(m, mf)
      whenever(pred(p)) {
        trigger.satisfiedBy(state) should ===(p)
      }
    }
  }

  behavior of "Trigger.flatten"

  it should "have one empty branch on Trigger.empty" in new Instances {
    compare(Trigger.empty, List(Branch(Seq.empty, Seq.empty)))
  }

  it should "flatten combination of conditions" in new Instances {
    compare(
      actual = Trigger(m1, m2, Trigger.and(m3, Trigger.and(m4, m5, m6))),
      expected = List(Branch(timers = Seq.empty, conditions = Seq(m1, m2, m3, m4, m5, m6)))
    )

    compare(
      actual = Trigger(m1, m2, Trigger.or(Trigger.and(m3, m4), Trigger.and(m5, m6))),
      expected = List(
        Branch(timers = Seq.empty, conditions = Seq(m1, m2, m3, m4)),
        Branch(timers = Seq.empty, conditions = Seq(m1, m2, m5, m6))
      )
    )

    compare(
      actual = Trigger.or(m1, m2, Trigger.and(m3, Trigger.or(m4, m5, m6))),
      expected = List(
        Branch(timers = Seq.empty, conditions = Seq(m1)),
        Branch(timers = Seq.empty, conditions = Seq(m2)),
        Branch(timers = Seq.empty, conditions = Seq(m3, m4)),
        Branch(timers = Seq.empty, conditions = Seq(m3, m5)),
        Branch(timers = Seq.empty, conditions = Seq(m3, m6))
      )
    )

    compare(
      actual = Trigger(m7, Trigger.or(
        Trigger.and(m4, Trigger.or(m1, Trigger.and(m2, m3))),
        Trigger.and(m5, m6)
      )),
      expected = List(
        Branch(timers = Seq.empty, conditions = Seq(m7, m4, m1)),
        Branch(timers = Seq.empty, conditions = Seq(m7, m4, m2, m3)),
        Branch(timers = Seq.empty, conditions = Seq(m7, m5, m6))
      )
    )
  }

  behavior of "!Trigger"

  it should "yield negated version for Trigger.Condition" in {
    !Trigger.Condition[TestModel](_.i == 1) should === (Some(Trigger.Condition[TestModel](_.i != 1)))
  }

  it should "obey De Morgan's laws" in {
    !Trigger.or(
      Trigger.and(Trigger.Condition[TestModel](_.s == "xyz"), Trigger.Condition[TestModel](_.i == 1)),
      Trigger.Condition[TestModel](_.i > 1)
    ) should === (Some(Trigger.and(
      Trigger.or(Trigger.Condition[TestModel](_.s != "xyz"), Trigger.Condition[TestModel](_.i != 1)),
      Trigger.Condition[TestModel](_.i <= 1)
    )))
  }

  it should "yield None for Trigger.Timer" in {
    !Trigger.Timer(CronExpression(
      minute = CronExpression.Range(0, 59),
      hour = CronExpression.Range(0, 23),
      dayOfMonth = CronExpression.Range(1, 31),
      month = CronExpression.Range(1, 12),
      dayOfWeek = CronExpression.Range(0, 6),
      year = None
    )) should === (None)

    !Trigger.Timer.dynamic(Duration.Zero, Set.empty)((_, _, _) => None) should === (None)
  }

  it should "yield None if any of nested standalone trigger is not Trigger.Condition" in {
    !Trigger.or(
      Trigger.and(
        Trigger.Condition[TestModel](_.s == "xyz"),
        Trigger.Condition[TestModel](_.i == 1),
        Trigger.Timer.dynamic(Duration.Zero, Set.empty)((_, _, _) => None)
      ),
      Trigger.Condition[TestModel](_.i > 1)
    ) should === (None)
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
