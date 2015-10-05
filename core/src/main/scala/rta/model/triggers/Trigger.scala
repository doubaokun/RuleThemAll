package rta.model.triggers

import scala.language.existentials
import android.content.{Context, Intent}
import cats.std.all._
import cats.syntax.all._
import java.util.{Date, UUID}
import scala.concurrent.duration.Duration
import scala.util.Try
import shapeless.HMap
import rta.common.{Requirement, Uses}
import rta.cron.CronExpression
import rta.logging.Logging
import rta.model._
import rta.model.triggers.functions.ModelFunction
import rta.{cron, logging}

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
  final case class Branch(conditions: Seq[Condition[_]], timers: Seq[Timer]) {
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
  final case class And private[Trigger](triggers: Seq[Trigger]) extends Trigger {
    protected def flatChildren: Seq[FlatResult] = triggers.flatMap(_.flatChildren)
  }

  /** Temporary trigger that denotes alternative of conditions.
    *
    * Note that each alternative results in separate [[Branch]].
    */
  final case class Or private[Trigger](triggers: Seq[Trigger]) extends Trigger {
    protected def flatChildren: Seq[FlatResult] =
      Seq(Right(triggers.flatMap(_.flatten)(collection.breakOut)))
  }

  /** Base for all triggers that should be part of a [[Branch]]. */
  sealed abstract class Standalone[M <: BaseModel] extends Trigger {
    def requires: Set[Requirement]
  }

  sealed abstract class Timer extends Standalone[Nothing] with Logging {
    override implicit lazy val logTag: logging.LogTag = {
      val name = s"Timer.${this.getClass.getSimpleName}"
      new logging.LogTag(name.substring(0, math.min(name.length, 23)))
    }

    /** Returns date at which rest of [[Branch]] conditions should be checked.
      *
      * Note that:
      *  - `None` means that further execution of rule should be suppressed
      *  - `Some(date, true)` means that timer should be fired again at given `date`
      *  - `Some(date, false)` means that at given `date` rest of triggers should be checked
      */
    def fireAt(context: Context, waitTime: Duration): Option[(Date, Boolean)] = {
      val from = new Date
      from.setTime(from.getTime + waitTime.toMillis)
      val date = fireAt(from, context).flatMap { case (d, flag) =>
        d.setTime(d.getTime - (d.getTime % (1000 * 60))) // we only want minute precision
        if (d.before(from)) fireAt(d, context) else Some(d -> flag)
      }
      log.info(s"Fire at $date")
      date
    }

    protected def fireAt(from: Date, context: Context): Option[(Date, Boolean)]

    protected def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))
  }

  object Timer {
    /** Returns date based on the [[cron.CronExpression]]. */
    final case class CronBased private[Timer](expr: CronExpression) extends Timer {
      protected def fireAt(from: Date, context: Context) = expr.nextDate(from).map(_ -> false)

      def requires: Set[Requirement] = Set.empty
    }

    /** Returns date that can be dynamically designated.
      *
      * @param requires set of [[rta.common.Requirement]] that can influence timer behaviour
      * @param recheckAfter duration after which timer should be checked again
      * @param fromContext function that accepts four arguments:
      *                     - `date` that is current time
      *                     - `timeWindow` that is equal to the [[recheckAfter]] field
      *                     - `context` that is [[android.content.Context]]
      *                    and returns date when timer is fulfilled.
      */
    final case class Dynamic private[Timer](requires: Set[Requirement], recheckAfter: Duration)
      (fromContext: (Date, Duration, Context) => Option[Date]) extends Timer {
      protected def fireAt(from: Date, context: Context) = {
        fromContext(from, recheckAfter, context).map(_ -> false).orElse {
          Some(new Date(from.getTime + recheckAfter.toMillis) -> true)
        }
      }
    }

    def prepareIntent(intent: Intent, rule: String, branchId: UUID, partial: Boolean): Unit =
      intent.setAction(s"rta.rule.timer/$rule/$branchId").putExtra("partial", partial)

    def unapply(intent: Intent): Option[(String, UUID, Boolean)] = {
      Option(intent.getAction).filter(_.startsWith("rta.rule.timer/")).flatMap { str =>
        Try {
          val splitted = str.stripPrefix("rta.rule.timer/").split('/')
          val ruleName = splitted.init.mkString("/")
          val branchId = UUID.fromString(splitted.last)
          val partial = intent.getBooleanExtra("partial", true)
          (ruleName, branchId, partial)
        }.toOption
      }
    }

    def apply(expr: CronExpression): Timer = CronBased(expr)

    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.DefaultArguments"))
    def dynamic(recheckAfter: Duration, requirements: Set[Requirement] = Set.empty)
      (fromContext: (Date, Duration, Context) => Option[Date]) =
      Dynamic(requirements, recheckAfter)(fromContext)
  }

  /** Standalone condition that gets model from a state and check it with [[ModelFunction]]. */
  final case class Condition[M <: BaseModel: BaseModelCompanion: Uses](function: ModelFunction[M])
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
