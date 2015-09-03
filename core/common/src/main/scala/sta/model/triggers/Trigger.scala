package sta.model.triggers

import scala.language.existentials
import android.content.Intent
import cats.std.all._
import cats.syntax.all._
import java.util.{UUID, Date}
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
    cross.result().sequence.map { branches =>
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

  sealed abstract class Standalone[M <: Model] extends Trigger

  sealed abstract class Timer extends Standalone[Nothing] {

    def fireAt(waitTime: Duration): Option[Date]

    def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))
  }
  
  object Timer {
    def setAction(intent: Intent, rule: String, branchId: UUID, partial: Boolean): Intent = {
      intent.setAction(s"sta.rule.timer/$rule/$branchId")
      intent.putExtra("partial", partial)
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

    def apply(expr: CronExpression): Timer = new Timer {
      def fireAt(waitTime: Duration) = {
        val from = new Date
        from.setTime(from.getTime + waitTime.toMillis)
        expr.nextDate(from)
      }
    }
  }

  case class Condition[M <: Model: ModelCompanion: Uses](function: ModelFunction[M]) extends Standalone[M] {
    def unary_! = copy(function = NotFunction(function))
    
    def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))

    def satisfiedBy(state: HMap[ModelKV]): Boolean = {
      val companion = implicitly[ModelCompanion[M]]
      import companion._
      state.get(Key).exists(function)
    }

    def requires: Set[Requirement] = implicitly[Uses[M]].requirements
  }
}
