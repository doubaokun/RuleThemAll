package sta.model

import scala.language.implicitConversions
import android.app.{PendingIntent, AlarmManager}
import android.content.{Intent, Context}
import cats._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, Validated}
import cats.std.all._
import cats.syntax.all._
import java.util.{Date, UUID}
import kj.android.common.{Toast, Notify, AppInfo}
import kj.android.common.SystemServices._
import kj.android.logging._
import scala.concurrent.duration._
import scala.util.control.NonFatal
import shapeless.HMap
import sta.common.Requirement
import sta.model.actions.Action
import sta.model.triggers.Trigger.Branch
import sta.model.triggers._
import sta.service.RulesExecutor

final case class Rule(name: String, branches: Seq[Trigger.Branch], actions: Seq[Action]) extends Logging {
  type Success = Unit
  type Fail = (String, Throwable)
  type FailNEL = NEL[Fail]
  type Result = Validated[FailNEL, Success]

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Var"))
  private[this] var executed = false

  private[this] lazy val (direct, withTimer) = {
    val directBuilder = Seq.newBuilder[Branch]
    val withTimerBuilder = Map.newBuilder[UUID, Branch]
    for(branch <- branches) {
      if (branch.timers.isEmpty) directBuilder += branch
      else withTimerBuilder += (UUID.randomUUID() -> branch)
    }
    (directBuilder.result(), withTimerBuilder.result())
  }

  private def executeRule(implicit ctx: RulesExecutor, logTag: LogTag, appInfo: AppInfo): Unit = {
    if (!executed) {
      log.info(s"Executing actions in rule $name")
      implicit val nelSemigroup: Semigroup[FailNEL] = SemigroupK[NEL].algebra[Fail]
      val combine = (_: Unit, _: Unit) => ()
      val result = actions.foldLeft(valid[FailNEL, Success](())) { case (acc, action) =>
        (acc |@| ctx.executeAction(action)) map combine
      }
      result.fold(
        errs => {
          (errs.head :: errs.tail).foreach { case (action, th) =>
            Logger.error(s"Error has occurred during running action $action in $name", th)
          }
          Notify(s"Failed to execute $name", Some(name)) // TODO add notification action
        }, _ => Toast(s"Rule $name executed successfully")
      )
      executed = true
    }
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

  def execute(state: HMap[ModelKV])(implicit ctx: RulesExecutor, logTag: LogTag, appInfo: AppInfo): Unit = {
    if (direct.exists(_.conditions.forall(_.satisfiedBy(state)))) executeRule
    else executed = false
  }
  
  def executeBranch(branchId: UUID, intent: Intent, state: HMap[ModelKV],
    timerFullyExecuted: Boolean)(implicit ctx: RulesExecutor, logTag: LogTag, appInfo: AppInfo) = {
    val branch = withTimer.get(branchId)
    branch.foreach(setAlarm(_, branchId, intent, alarmManager, 60.seconds))
    if (timerFullyExecuted && branch.exists(_.conditions.forall(_.satisfiedBy(state)))) executeRule
    else if (timerFullyExecuted) executed = false
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
    case Rule(`name`, _, _) => true
    case _ => false
  }
}
