package sta.model.triggers

import scala.language.existentials
import cats.std.all._
import cats.syntax.all._
import kj.android.concurrent.Task
import scala.util.{Try, Success}
import shapeless.HMap
import sta.common.{Requirement, Uses}
import sta.model._
import sta.model.triggers.functions.{ModelFunction, NotFunction}

sealed abstract class Trigger {
  protected type FlatResult = Either[List[Trigger.Standalone[_]], List[Trigger.Branch]]

  def flatChildren: Seq[FlatResult]

  final def flatten: Seq[Trigger.Branch] = {
    val signals = Seq.newBuilder[Trigger.Signal]
    val triggers = Seq.newBuilder[Trigger.Condition[_]]
    val cross = List.newBuilder[List[Trigger.Branch]]
    flatChildren.foreach {
      case Left(xs) => xs.foreach {
        case s: Trigger.Signal => signals += s
        case t: Trigger.Condition[_] => triggers += t
      }
      case Right(branches) => cross += branches
    }
    cross.result().sequence.map { branches =>
      Trigger.Branch(
        signal = Trigger.Signal.join(signals.result() ++ branches.flatMap(_.signal)),
        conditions = triggers.result() ++ branches.flatMap(_.conditions)
      )
    }
  }
}

object Trigger {
  case class Branch(signal: Option[Signal] = None, conditions: Seq[Condition[_]] = Seq.empty)

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

  class Signal(rawTask: Task[Unit]) extends Standalone[Nothing] {
    private def task: Task[Unit] = rawTask

    def start(onComplete: Try[Unit] => Unit): Unit = rawTask.run(onComplete)

    def stop(): Unit = rawTask.cancel(true)

    def flatChildren: Seq[FlatResult] = Seq(Left(List(this)))
  }
  
  object Signal {
    def apply(task: Task[Unit]): Signal = new Signal(task)

    def join(seq: Seq[Signal]): Option[Signal] =
      if (seq.isEmpty) None else Some(new Signal(Task.join(seq.map(_.task))))
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
