package sta.model

import scala.language.implicitConversions
import android.content.Context
import cats._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, Validated}
import cats.std.all._
import cats.syntax.all._
import kj.android.common.{Notify, AppInfo}
import kj.android.logging._
import scala.util.control.NonFatal
import shapeless.HMap
import sta.common.Requirement
import sta.model.actions.Action
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

  // TODO rete
  def tryExecute(state: HMap[ModelKV])(implicit ctx: Context): Either[Rule, Result] = {
    if (branches.exists(_.triggers.forall(_.satisfiedBy(state)))) {
      implicit val nelSemigroup: Semigroup[FailNEL] = SemigroupK[NEL].algebra[Fail]
      val combine = (_: Unit, _: Unit) => ()
      Right(
        actions.foldLeft(valid[FailNEL, Success](())) { case (acc, action) =>
          (acc |@| executeAction(action)) map combine
        }
      )
    } else Left(this)
  }

  def execute(state: HMap[ModelKV])(implicit ctx: Context, logTag: LogTag, appInfo: AppInfo): Unit = {
    tryExecute(state).fold(
      _ => (),
      v => v.fold(errs => {
        (errs.head :: errs.tail).foreach { case (action, th) =>
          Logger.error(s"Error has occurred during running action $action in $name", th)
        }
        Notify(s"Failed to execute $name", Some(name))
      }, _ => Notify(s"Rule $name executed successfully"))
    )
  }

  lazy val requires: Set[Requirement] = (for {
    branch <- branches
    trigger <- branch.triggers
    requirement <- trigger.requires
  } yield requirement).toSet

  override def hashCode(): Int = name.hashCode

  override def equals(o: Any): Boolean = o match {
    case Rule(`name`, _, _) => true
    case _ => false
  }
}
