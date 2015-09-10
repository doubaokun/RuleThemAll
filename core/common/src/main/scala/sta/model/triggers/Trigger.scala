package sta.model.triggers

import scala.language.existentials
import android.content.{Context, Intent}
import cats.std.all._
import cats.syntax.all._
import java.util.{Date, UUID}
import kj.android.cron.CronExpression
import scala.concurrent.duration.Duration
import scala.util.Try
import shapeless.HMap
import sta.common.{Requirement, Uses}
import sta.model._
import sta.model.triggers.functions.{ModelFunction, NotFunction}

sealed abstract class Trigger {
  protected type FlatResult = Either[List[Trigger.Standalone[_]], List[Trigger.Branch]]

  def flatChildren: Seq[FlatResult]

  final def flatten: Seq[Trigger.Branch] = {
    val timers = IndexedSeq.newBuilder[Trigger.Timer]
    val triggers = Seq.newBuilder[Trigger.Condition[_]]
    val cross = List.newBuilder[List[Trigger.Branch]]
    flatChildren.foreach {
      case Left(xs) => xs.foreach {
        case s: Trigger.Timer => timers += s
        case t: Trigger.Condition[_] => triggers += t
      }
      case Right(branches) => cross += branches
    }
    cross.result().sequenceU.map { branches =>
      Trigger.Branch(
        timers = timers.result() ++ branches.flatMap(_.timers),
        conditions = triggers.result() ++ branches.flatMap(_.conditions)
      )
    }
  }
}

object Trigger {
  case class Branch(timers: Seq[Timer] = Seq.empty, conditions: Seq[Condition[_]] = Seq.empty)

  def empty = Empty

  def apply(trigger: Trigger, triggers: Trigger*): Trigger = {
    if (triggers.nonEmpty) And(trigger +: triggers)
    else trigger
  }

  def and(trigger1: Trigger, trigger2: Trigger, triggers: Trigger*): And = {
    And(trigger1 +: trigger2 +: triggers)
  }

  def or(trigger1: Trigger, trigger2: Trigger, triggers: Trigger*): Or = {
    Or(trigger1 +: trigger2 +: triggers)
  }

  object Empty extends Trigger {
    def flatChildren: Seq[FlatResult] = Seq.empty
  }

  case class And private[Trigger](triggers: Seq[Trigger]) extends Trigger {
    def flatChildren: Seq[FlatResult] = triggers.flatMap(_.flatChildren)
  }

  case class Or private[Trigger](triggers: Seq[Trigger]) extends Trigger {
    def flatChildren: Seq[FlatResult] = Seq(Right(triggers.flatMap(_.flatten)(collection.breakOut)))
  }

  sealed abstract class Standalone[M <: BaseModel] extends Trigger {
    def requires: Set[Requirement]
  }

  sealed abstract class Timer extends Standalone[Nothing] {
    /** Returns date at which rest of [[sta.model.Rule]] triggers should be checked.
      *
      * Note that:
      *  - `None` means that further execution of rule should be suppressed
      *  - `Some(date, true)` means that timer should be fired again at given `date`
      *  - `Some(date, false)` means that at given `date` rest of triggers should be checked
      */
    def fireAt(context: Context, waitTime: Duration): Option[(Date, Boolean)]

    def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))
  }

  object Timer {
    case class CronBased(expr: CronExpression) extends Timer {
      def requires: Set[Requirement] = Set.empty

      def fireAt(context: Context, waitTime: Duration) = {
        val from = new Date
        from.setTime(from.getTime + waitTime.toMillis)
        expr.nextDate(from).map(_ -> false)
      }
    }

    case class Dynamic(requires: Set[Requirement], recheckAfter: Duration,
      fromContext: (Date, Duration, Duration, Context) => Option[Date]) extends Timer {
      def fireAt(context: Context, waitTime: Duration) = {
        val from = new Date
        from.setTime(from.getTime + waitTime.toMillis)
        fromContext(from, waitTime, recheckAfter, context).map(_ -> false).orElse {
          Some(new Date(from.getTime + recheckAfter.toMillis) -> true)
        }
      }
    }

    def setAction(intent: Intent, rule: String, branchId: UUID, partial: Boolean): Unit = {
      intent.setAction(s"sta.rule.timer/$rule/$branchId").putExtra("partial", partial)
    }

    def unapply(intent: Intent): Option[(String, UUID, Boolean)] = {
      Option(intent.getAction).filter(_.startsWith("sta.rule.timer/")).flatMap { str =>
        Try {
          val splitted = str.stripPrefix("sta.rule.timer/").split('/')
          val ruleName = splitted.init.mkString("/")
          val branchId = UUID.fromString(splitted.last)
          val partial = intent.getBooleanExtra("partial", true)
          (ruleName, branchId, partial)
        }.toOption
      }
    }

    def apply(expr: CronExpression): Timer = CronBased(expr)

    def dynamic(recheckAfter: Duration, requirements: Set[Requirement] = Set.empty)
      (fromContext: (Date, Duration, Duration, Context) => Option[Date]) = Dynamic(requirements, recheckAfter, fromContext)
  }

  case class Condition[M <: BaseModel: BaseModelCompanion: Uses](function: ModelFunction[M]) extends Standalone[M] {
    def unary_! = copy(function = NotFunction(function))

    def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))

    def satisfiedBy(state: HMap[ModelKV]): Boolean = {
      val companion = implicitly[BaseModelCompanion[M]]
      import companion._
      state.get(Key).exists(exists(_, function))
    }

    def requires: Set[Requirement] = implicitly[Uses[M]].requirements
  }
}
