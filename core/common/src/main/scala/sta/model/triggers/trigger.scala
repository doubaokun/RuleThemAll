package sta.model.triggers

import cats.std.all._
import cats.syntax.all._
import shapeless.HMap
import sta.common.{Requirement, Uses}
import sta.model._
import sta.model.triggers.functions.{ModelFunction, NotFunction}

sealed abstract class Trigger {
  protected type FlatResult = Either[List[Trigger.Atomic[_]], List[Trigger.Branch]]

  def flatChildren: Seq[FlatResult]

  final def flatten: Seq[Trigger.Branch] = {
    val prefix = Seq.newBuilder[Trigger.Atomic[_]]
    val cross = List.newBuilder[List[Trigger.Branch]]
    flatChildren.foreach {
      case Left(triggers) => prefix ++= triggers
      case Right(branches) => cross += branches
    }
    cross.result().sequence.map { branches =>
      Trigger.Branch(prefix.result() ++ branches.flatMap(_.triggers))
    }
  }

  def satisfiedBy(state: HMap[ModelKV]): Boolean

  def requires: Set[Requirement]
}

object Trigger {
  case class Branch(triggers: Seq[Trigger.Atomic[_]])

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

  def xor(lhs: Trigger, rhs: Trigger): Xor = Xor(lhs, rhs)

  object Empty extends Trigger {
    def flatChildren: Seq[FlatResult] = Seq.empty

    def satisfiedBy(state: HMap[ModelKV]): Boolean = true

    def requires: Set[Requirement] = Set.empty
  }

  case class And private[Trigger](triggers: Seq[Trigger]) extends Trigger {
    def flatChildren: Seq[FlatResult] = triggers.flatMap(_.flatChildren)

    def satisfiedBy(state: HMap[ModelKV]): Boolean = triggers.forall(_.satisfiedBy(state))

    def requires: Set[Requirement] = triggers.flatMap(_.requires)(collection.breakOut)
  }

  case class Or private[Trigger](triggers: Seq[Trigger]) extends Trigger {
    def flatChildren: Seq[FlatResult] = Seq(Right(triggers.flatMap(_.flatten)(collection.breakOut)))

    def satisfiedBy(state: HMap[ModelKV]): Boolean = triggers.exists(_.satisfiedBy(state))

    def requires: Set[Requirement] = triggers.flatMap(_.requires)(collection.breakOut)
  }

  case class Xor private[Trigger](lhs: Trigger, rhs: Trigger) extends Trigger {
    def flatChildren: Seq[FlatResult] = {
      val branches = List(
        Branch(
          lhs.flatten.flatMap(_.triggers) ++ rhs.flatten.flatMap(_.triggers.map { t =>
            val casted = t.asInstanceOf[Atomic[Model]]
            Atomic(NotFunction(casted.function))(casted.companion, casted.uses)
          })
        ),
        Branch(
          lhs.flatten.flatMap(_.triggers.map { t =>
            val casted = t.asInstanceOf[Atomic[Model]]
            Atomic(NotFunction(casted.function))(casted.companion, casted.uses)
          }) ++ rhs.flatten.flatMap(_.triggers)
        )
      )
      Seq(Right(branches))
    }

    def satisfiedBy(state: HMap[ModelKV]): Boolean = {
      val l = lhs.satisfiedBy(state)
      val r = rhs.satisfiedBy(state)

      (l && !r) || (!l && r)
    }

    def requires: Set[Requirement] = lhs.requires ++ rhs.requires
  }

  case class Atomic[M <: Model: ModelCompanion: Uses](function: ModelFunction[M]) extends Trigger {
    protected[Trigger] def companion = implicitly[ModelCompanion[M]]

    protected[Trigger] def uses = implicitly[Uses[M]]

    def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))

    def satisfiedBy(state: HMap[ModelKV]): Boolean = {
      val companion = implicitly[ModelCompanion[M]]
      import companion._
      state.get(Key).exists(function)
    }

    def requires: Set[Requirement] = implicitly[Uses[M]].requirements

    def withRequirements(req: Requirement, reqs: Requirement*): Atomic[M] =
      new Atomic(function) {
        override def requires: Set[Requirement] = implicitly[Uses[M]].requirements ++ reqs + req
      }
  }
}
