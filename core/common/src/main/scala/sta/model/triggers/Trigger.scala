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
import sta.model.triggers.functions.ModelFunction

/** Base for all triggers either temporary or standalone. */
sealed abstract class Trigger {
  protected type FlatResult = Either[List[Trigger.Standalone[_]], List[Trigger.Branch]]

  protected def flatChildren: Seq[FlatResult]

  /** Flattens arbitrary tree of [[Trigger]] into sequence of [[Trigger.Branch]]. */
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

  /** Denotes set of conditions with set of timers that
    * indicates date when such conditions should be fulfilled.
    */
  case class Branch(timers: Seq[Timer] = Seq.empty, conditions: Seq[Condition[_]] = Seq.empty) {
    lazy val requires: Set[Requirement] =
      (timers.flatMap(_.requires) ++ conditions.flatMap(_.requires))(collection.breakOut)
  }

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

  /** Special empty trigger that denotes empty branch. */
  object Empty extends Trigger {
    protected def flatChildren: Seq[FlatResult] = Seq.empty
  }

  /** Temporary trigger that denotes conjunction of conditions. */
  case class And private[Trigger](triggers: Seq[Trigger]) extends Trigger {
    protected def flatChildren: Seq[FlatResult] = triggers.flatMap(_.flatChildren)
  }

  /** Temporary trigger that denotes alternative of conditions.
    *
    * Note that each alternative results in separate [[Branch]].
    */
  case class Or private[Trigger](triggers: Seq[Trigger]) extends Trigger {
    protected def flatChildren: Seq[FlatResult] =
      Seq(Right(triggers.flatMap(_.flatten)(collection.breakOut)))
  }

  /** Base for all triggers that should be part of a [[Branch]]. */
  sealed abstract class Standalone[M <: BaseModel] extends Trigger {
    def requires: Set[Requirement]
  }

  sealed abstract class Timer extends Standalone[Nothing] {
    /** Returns date at which rest of [[Branch]] conditions should be checked.
      *
      * Note that:
      *  - `None` means that further execution of rule should be suppressed
      *  - `Some(date, true)` means that timer should be fired again at given `date`
      *  - `Some(date, false)` means that at given `date` rest of triggers should be checked
      */
    def fireAt(context: Context, waitTime: Duration): Option[(Date, Boolean)]

    protected def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))
  }

  object Timer {
    /** Returns date based on the [[kj.android.cron.CronExpression]]. */
    case class CronBased private[Timer](expr: CronExpression) extends Timer {
      def requires: Set[Requirement] = Set.empty

      def fireAt(context: Context, waitTime: Duration) = {
        val from = new Date
        from.setTime(from.getTime + waitTime.toMillis)
        val d = expr.nextDate(from).map(_ -> false)
        android.util.Log.i("Timer.CronBased", s"$d")
        d
      }
    }

    /** Returns date that can be dynamically designated.
      *
      * @param requires set of [[sta.common.Requirement]] that can influence timer behaviour
      * @param recheckAfter duration after which timer should be checked again
      * @param fromContext function that accepts four arguments:
      *                     - `date` that is current time shifted with `waitTime`
      *                     - `waitTime`
      *                     - `timeWindow` that is equal to the [[recheckAfter]] field
      *                     - `context` that is [[android.content.Context]]
      *                    and returns date when timer is fulfilled.
      */
    case class Dynamic private[Timer](requires: Set[Requirement], recheckAfter: Duration,
      fromContext: (Date, Duration, Duration, Context) => Option[Date]) extends Timer {
      def fireAt(context: Context, waitTime: Duration) = {
        val from = new Date
        from.setTime(from.getTime + waitTime.toMillis)
        val d = fromContext(from, waitTime, recheckAfter, context).map(_ -> false).orElse {
          Some(new Date(from.getTime + recheckAfter.toMillis) -> true)
        }
        android.util.Log.i("Timer.Dynamic", s"$d")
        d
      }
    }

    def prepareIntent(intent: Intent, rule: String, branchId: UUID, partial: Boolean): Unit =
      intent.setAction(s"sta.rule.timer/$rule/$branchId").putExtra("partial", partial)

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
      (fromContext: (Date, Duration, Duration, Context) => Option[Date]) =
      Dynamic(requirements, recheckAfter, fromContext)
  }

  /** Standalone condition that gets model from a state and check it with [[ModelFunction]]. */
  case class Condition[M <: BaseModel: BaseModelCompanion: Uses](function: ModelFunction[M])
    extends Standalone[M] {
    protected def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))

    def satisfiedBy(state: HMap[ModelKV]): Boolean = {
      val companion = implicitly[BaseModelCompanion[M]]
      import companion._
      state.get(Key).exists(exists(_, function))
    }

    def requires: Set[Requirement] = implicitly[Uses[M]].requirements
  }
}
