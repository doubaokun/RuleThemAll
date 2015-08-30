package sta.model.triggers

import cats.std.all._
import cats.syntax.all._
import shapeless.HMap
import sta.common.{Requirement, Uses}
import sta.model._
import sta.model.triggers.functions.{ModelFunction, NotFunction}

sealed abstract class Condition {
  protected type FlatResult = Either[List[Condition.Standalone[_]], List[Condition.Branch]]

  def flatChildren: Seq[FlatResult]

  final def flatten: Seq[Condition.Branch] = {
    val triggers = Seq.newBuilder[Condition.Trigger[_]]
    val cross = List.newBuilder[List[Condition.Branch]]
    flatChildren.foreach {
      case Left(xs) => xs.foreach {
        case t: Condition.Trigger[_] => triggers += t
      }
      case Right(branches) => cross += branches
    }
    cross.result().sequence.map { branches =>
      Condition.Branch(
        triggers = triggers.result() ++ branches.flatMap(_.triggers)
      )
    }
  }
}

object Condition {
  case class Branch(triggers: Seq[Trigger[_]] = Seq.empty)

  def empty = Empty

  def apply(trigger: Condition, triggers: Condition*): Condition = {
    if (triggers.nonEmpty) And(trigger +: triggers)
    else trigger
  }

  def and(trigger1: Condition, trigger2: Condition, triggers: Condition*): And = {
    And(trigger1 +: trigger2 +: triggers)
  }

  def or(trigger1: Condition, trigger2: Condition, triggers: Condition*): Or = {
    Or(trigger1 +: trigger2 +: triggers)
  }

  def xor(lhs: Condition, rhs: Condition): Xor = Xor(lhs, rhs)

  object Empty extends Condition {
    def flatChildren: Seq[FlatResult] = Seq.empty
  }

  case class And private[Condition](triggers: Seq[Condition]) extends Condition {
    def flatChildren: Seq[FlatResult] = triggers.flatMap(_.flatChildren)
  }

  case class Or private[Condition](triggers: Seq[Condition]) extends Condition {
    def flatChildren: Seq[FlatResult] = Seq(Right(triggers.flatMap(_.flatten)(collection.breakOut)))
  }

  case class Xor private[Condition](lhs: Condition, rhs: Condition) extends Condition {
    def flatChildren: Seq[FlatResult] = {
      val lhsF = lhs.flatten
      val rhsF = rhs.flatten
      val branches = List(
        Branch(
          triggers = lhsF.flatMap(_.triggers) ++ rhsF.flatMap(_.triggers.map(!_))
        ),
        Branch(
          triggers = lhsF.flatMap(_.triggers.map(!_)) ++ rhsF.flatMap(_.triggers)
        )
      )
      Seq(Right(branches))
    }
  }

  sealed abstract class Standalone[M <: Model] extends Condition

  case class Trigger[M <: Model: ModelCompanion: Uses](function: ModelFunction[M]) extends Standalone {
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
