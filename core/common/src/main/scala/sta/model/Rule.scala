package sta.model

import scala.language.implicitConversions
import android.app.{PendingIntent, AlarmManager}
import android.content.{Intent, Context}
import cats._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, Validated}
import cats.std.all._
import cats.syntax.all._
import java.util.UUID
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

case class Rule(name: String, branches: Seq[Trigger.Branch], actions: Seq[Action]) extends Logging {
  type Success = Unit
  type Fail = (String, Throwable)
  type FailNEL = NEL[Fail]
  type Result = Validated[FailNEL, Success]

  @inline private def executeAction(a: Action)(implicit ctx: Context): Result = try {
    Validated.valid(a.execute())
  } catch {
    case NonFatal(t) => Validated.invalidNel(a.name -> t)
  }

  private[this] var executed = false

  private[this] lazy val (direct, withTimer) = {
    val directBuilder = Seq.newBuilder[Branch]
    val withTimerBuilder = Map.newBuilder[UUID, Branch]
    branches.foreach {
      case d if d.timers.isEmpty => directBuilder += d
      case t => withTimerBuilder += (UUID.randomUUID() -> t)
    }
    (directBuilder.result(), withTimerBuilder.result())
  }

  private def executeRule(implicit ctx: Context, logTag: LogTag, appInfo: AppInfo): Unit = {
    if (!executed) {
      log.info(s"Executing actions in rule $name")
      implicit val nelSemigroup: Semigroup[FailNEL] = SemigroupK[NEL].algebra[Fail]
      val combine = (_: Unit, _: Unit) => ()
      val result = actions.foldLeft(valid[FailNEL, Success](())) { case (acc, action) =>
        (acc |@| executeAction(action)) map combine
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

  private def setTimer(branch: Branch, branchId: UUID, intent: Intent,
    alarmManager: AlarmManager, waitTime: Duration)(implicit ctx: Context): Unit = {
    val dates = for {
      timer <- branch.timers
      (date, partial) <- timer.fireAt(context = ctx, waitTime = waitTime)
    } yield {
      date.setTime(date.getTime - (date.getTime % (1000 * 60))) // we only want minute precision
        (date, partial)
    }
    if (dates.length == branch.timers.length) {
      val (date, partial) = dates.minBy(_._1.getTime)
      if (partial || dates.exists(d => d._2 || d._1 != date))
        Trigger.Timer.setAction(intent, name, branchId, partial = true)
      else
        Trigger.Timer.setAction(intent, name, branchId, partial = false)
      alarmManager.setExact(AlarmManager.RTC_WAKEUP, date.getTime,
        PendingIntent.getService(ctx, 0, intent, PendingIntent.FLAG_ONE_SHOT))
    }
  }

  def execute(state: HMap[ModelKV])(implicit ctx: Context, logTag: LogTag, appInfo: AppInfo): Unit = {
    if (direct.exists(_.conditions.forall(_.satisfiedBy(state)))) executeRule
    else executed = false
  }

  def setTimers(intent: Intent)(implicit ctx: Context, logTag: LogTag, appInfo: AppInfo): Seq[Intent] =  {
    val manager = alarmManager
    withTimer.map { case (id, branch) =>
      setTimer(branch, id, intent, manager, 0.millis)
      intent
    }(collection.breakOut)
  }
  
  def executeBranch(branchId: UUID, intent: Intent, state: HMap[ModelKV],
    timerFullyExecuted: Boolean)(implicit ctx: Context, logTag: LogTag, appInfo: AppInfo) = {
    val branch = withTimer.get(branchId)
    branch.foreach(setTimer(_, branchId, intent, alarmManager, 60.seconds))
    if (timerFullyExecuted && branch.exists(_.conditions.forall(_.satisfiedBy(state)))) executeRule
    else if (timerFullyExecuted) executed = false
  }

  lazy val requires: Set[Requirement] = {
    val requirements = Set.newBuilder[Requirement]
    for (branch <- branches) {
      for (condition <- branch.conditions) requirements ++= condition.requires
      for (timer <- branch.timers) requirements ++= timer.requires
    }
    requirements.result()
  }

  override def hashCode(): Int = name.hashCode

  override def equals(o: Any): Boolean = o match {
    case Rule(`name`, _, _) => true
    case _ => false
  }
}
