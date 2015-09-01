package sta.model

import scala.language.implicitConversions
import android.content.Context
import cats._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, Validated}
import cats.std.all._
import cats.syntax.all._
import kj.android.common.{Notify, AppInfo}
import kj.android.concurrent.Task
import kj.android.logging._
import scala.util.{Failure, Success}
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
    log.info(s"Executing action ${a.name}")
    Validated.valid(a.execute())
  } catch {
    case NonFatal(t) => Validated.invalidNel(a.name -> t)
  }

  private[this] val (direct, withSignal) = branches.partition(_.signal.isEmpty)

  private def executeRule(implicit ctx: Context, logTag: LogTag, appInfo: AppInfo): Unit = {
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
        Notify(s"Failed to execute $name", Some(name))
      }, _ => Notify(s"Rule $name executed successfully")
    )
  }

  def execute(state: HMap[ModelKV])(implicit ctx: Context, logTag: LogTag, appInfo: AppInfo): Unit = {
    if (direct.exists(_.conditions.forall(_.satisfiedBy(state)))) executeRule
  }

  def execute(stateHolder: () => HMap[ModelKV])(implicit ctx: Context, logTag: LogTag, appInfo: AppInfo): Unit = {
    for {
      branch <- withSignal
      signal <- branch.signal
    } signal.start {
      case Success(_) =>
        val state = stateHolder()
        if (branch.conditions.forall(_.satisfiedBy(state))) executeRule
      case Failure(th) =>
        Logger.error(s"Error has occurred during checking conditions in $name", th)
        Notify(s"Failed to execute $name", Some(name))
    }
  }

  lazy val requires: Set[Requirement] = (for {
    branch <- branches
    trigger <- branch.conditions
    requirement <- trigger.requires
  } yield requirement).toSet

  override def hashCode(): Int = name.hashCode

  override def equals(o: Any): Boolean = o match {
    case Rule(`name`, _, _) => true
    case _ => false
  }
}
