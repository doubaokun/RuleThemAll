package rta.model

import scala.language.implicitConversions
import android.app.{AlarmManager, PendingIntent}
import android.content.{Context, Intent}
import cats._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, Validated}
import cats.std.all._
import cats.syntax.all._
import java.util.UUID
import scala.concurrent.duration._
import shapeless.HMap
import rta.common.SystemServices._
import rta.common.{AppInfo, Notify, Requirement, Toast}
import rta.logging._
import rta.model.actions.Action
import rta.model.triggers.Trigger.Branch
import rta.model.triggers._
import rta.service.RulesExecutor
import spire.math.UByte

final case class Rule(name: String, priority: UByte, branches: Seq[Trigger.Branch], actions: Seq[Action]) extends Logging {
  type Success = Unit
  type Fail = (String, Throwable)
  type FailNEL = NEL[Fail]
  type Result = Validated[FailNEL, Success]

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Var"))
  @volatile private[rta] var executed = false // FIXME

  val (direct, withTimer) = {
    val directBuilder = Seq.newBuilder[Branch]
    val withTimerBuilder = Map.newBuilder[UUID, Branch]
    for(branch <- branches) {
      if (branch.timers.isEmpty) directBuilder += branch
      else withTimerBuilder += (branch.uuid -> branch)
    }
    (directBuilder.result(), withTimerBuilder.result())
  }

  private def setAlarm(branch: Branch, branchId: UUID, intent: Intent,
    alarmManager: AlarmManager, waitTime: Duration)(implicit ctx: Context): Option[PendingIntent] = {
    val dates = for {
      timer <- branch.timers
      (date, partial) <- timer.fireAt(context = ctx, waitTime = waitTime).toList
    } yield (date, partial)
    if (dates.length == branch.timers.length) {
      val (date, partial) = dates.minBy(_._1.getTime)
      if (partial || dates.exists(d => d._2 || d._1 != date))
        Trigger.Timer.prepareIntent(intent, name, branchId, partial = true)
      else
        Trigger.Timer.prepareIntent(intent, name, branchId, partial = false)
      val pending = PendingIntent.getService(ctx, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT)
      alarmManager.setExact(AlarmManager.RTC_WAKEUP, date.getTime, pending)
      Some(pending)
    } else None
  }

  def prepare()(implicit ctx: Context): Unit = actions.foreach(_.prepare())

  def satisfiedBy(state: HMap[ModelKV]): Boolean = {
    val succeeded = direct.exists(_.conditions.forall(_.satisfiedBy(state)))
    if (executed && !succeeded) executed = false
    succeeded
  }

  def execute(implicit ctx: RulesExecutor, appInfo: AppInfo): Unit = {
    log.info(s"Executing actions in rule $name")
    implicit val nelSemigroup: Semigroup[FailNEL] = SemigroupK[NEL].algebra[Fail]
    val combine = (_: Unit, _: Unit) => ()
    val result = actions.foldLeft(valid[FailNEL, Success](())) { case (acc, action) =>
      (acc |@| ctx.executeAction(action)) map combine
    }
    result.fold(
      errs => {
        (errs.head :: errs.tail).foreach { case (action, th) =>
          log.error(s"Error has occurred during running action $action in $name", th)
        }
        Notify(s"Failed to execute $name", Some(name)) // TODO add notification action
      }, _ => Toast(s"Rule $name executed successfully")
    )
    executed = true
  }
  
  def executeBranch(branchId: UUID, intent: Intent, state: HMap[ModelKV],
    timerFullyExecuted: Boolean)(implicit ctx: RulesExecutor, appInfo: AppInfo) = {
    for (branch <- withTimer.get(branchId)) {
      setAlarm(branch, branchId, intent, alarmManager, 60.seconds)
      if (timerFullyExecuted) branch.conditions.forall(_.satisfiedBy(state)) match {
        case true => execute
        case false => executed = false // rule was executed previously but now failed to match so update flag
      }
    }
  }

  lazy val requires: Set[Requirement] = branches.flatMap(_.requires)(collection.breakOut)

  def setAlarms(base: Intent)(implicit ctx: Context): Seq[PendingIntent] =  {
    val manager = alarmManager
    withTimer.flatMap { case (id, branch) =>
      setAlarm(branch, id, base.cloneFilter(), manager, Duration.Zero).toList
    }(collection.breakOut)
  }

  def setAlarmsFor(requirement: Requirement, base: Intent)(implicit ctx: Context): Seq[PendingIntent] = {
    val manager = alarmManager
    withTimer.flatMap {
      case (id, branch) if branch.requires.contains(requirement) =>
        setAlarm(branch, id, base.cloneFilter(), manager, Duration.Zero).toList
      case _ =>
        Nil
    }(collection.breakOut)
  }

  override def hashCode(): Int = name.hashCode

  override def equals(o: Any): Boolean = o match {
    case Rule(`name`, _, _, _) => true
    case _ => false
  }
}
