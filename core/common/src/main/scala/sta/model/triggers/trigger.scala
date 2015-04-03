package sta.model.triggers

import kj.android.common.UsedFeatures
import shapeless.HMap
import sta.model._
import sta.model.triggers.functions.ModelFunction

import scala.annotation.tailrec
import scala.collection.immutable
import scala.language.experimental.macros

object Trigger {
  // we know that trigger's seq is not empty by using parser rules
  def apply(triggers: immutable.Seq[Trigger]): Trigger = {
    @tailrec def construct(acc: Trigger, rest: immutable.Seq[Trigger]): Trigger = {
      if (rest.isEmpty) acc
      else construct(AndTrigger(acc, rest.head), rest.tail)
    }

    construct(triggers.head, triggers.tail)
  }

  // we know that trigger's seq has at least two elements by using parser rules
  def and(triggers: immutable.Seq[Trigger]): AndTrigger = {
    @tailrec def construct(acc: AndTrigger, rest: immutable.Seq[Trigger]): AndTrigger = {
      if (rest.isEmpty) acc
      else construct(AndTrigger(acc, rest.head), rest.tail)
    }

    val t = triggers.tail
    construct(AndTrigger(triggers.head, t.head), t.tail)
  }

  // we know that trigger's seq has at least two elements by using parser rules
  def or(triggers: immutable.Seq[Trigger]): OrTrigger = {
    @tailrec def construct(acc: OrTrigger, rest: immutable.Seq[Trigger]): OrTrigger = {
      if (rest.isEmpty) acc
      else construct(OrTrigger(acc, rest.head), rest.tail)
    }

    val t = triggers.tail
    construct(OrTrigger(triggers.head, t.head), t.tail)
  }

  // we know that trigger's seq has at least two elements by using parser rules
  def xor(triggers: immutable.Seq[Trigger]): XorTrigger = {
    @tailrec def construct(acc: XorTrigger, rest: immutable.Seq[Trigger]): XorTrigger = {
      if (rest.isEmpty) acc
      else construct(XorTrigger(acc, rest.head), rest.tail)
    }

    val t = triggers.tail
    construct(XorTrigger(triggers.head, t.head), t.tail)
  }
}

sealed abstract class Trigger {
  def satisfiedBy(state: HMap[ModelKV]): Boolean

  def uses: Set[String]
}

sealed abstract class LogicOpTrigger extends Trigger { // TODO optimize ?
  def lhs: Trigger

  def rhs: Trigger

  def uses: Set[String] = lhs.uses ++ rhs.uses
}

case class AndTrigger(lhs: Trigger, rhs: Trigger) extends LogicOpTrigger {
  def satisfiedBy(state: HMap[ModelKV]): Boolean = lhs.satisfiedBy(state) && rhs.satisfiedBy(state)
}

case class OrTrigger(lhs: Trigger, rhs: Trigger) extends LogicOpTrigger {
  def satisfiedBy(state: HMap[ModelKV]): Boolean = lhs.satisfiedBy(state) || rhs.satisfiedBy(state)
}

case class XorTrigger(lhs: Trigger, rhs: Trigger) extends LogicOpTrigger {
  def satisfiedBy(state: HMap[ModelKV]): Boolean = {
    val l = lhs.satisfiedBy(state)
    val r = rhs.satisfiedBy(state)

    (l && !r) || (!l && r)
  }
}

case class AtomicTrigger[M <: Model: ModelCompanion: UsedFeatures](function: ModelFunction[M]) extends Trigger {
  def satisfiedBy(state: HMap[ModelKV]): Boolean = {
    val companion = implicitly[ModelCompanion[M]]
    import companion._
    state.get(Key).exists(function)
  }

  def uses: Set[String] = Set(implicitly[UsedFeatures[M]].category)
}