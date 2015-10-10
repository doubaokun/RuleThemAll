package rta.service

import java.util.UUID
import rta.model.triggers.Trigger
import rta.model.{BaseModel, Rule}
import scala.annotation.tailrec
import scala.collection.mutable

@SuppressWarnings(Array(
  "org.brianmckenna.wartremover.warts.MutableDataStructures",
  "org.brianmckenna.wartremover.warts.Null",
  "org.brianmckenna.wartremover.warts.Var"
))
sealed class ExecutionNetwork {
  type ConflictSet = mutable.Set[Rule]
  type RuleContext = (String, UUID)

  sealed trait Node {
    def activate(resultSoFar: Boolean, context: RuleContext, model: BaseModel, set: ConflictSet): Boolean
  }

  private[this] object Node {
    final case class Alpha(condition: Trigger.Condition[BaseModel],
      successors: mutable.AnyRefMap[RuleContext, Node]) extends Node {
      private[this] var satisfied = false
      private[this] var last: BaseModel = null

      override def equals(o: Any): Boolean = o match {
        case other: Alpha => condition.equals(other.condition)
        case _ => false
      }

      override def hashCode(): Int = condition.hashCode()

      def activate(resultSoFar: Boolean, context: RuleContext, model: BaseModel, set: ConflictSet): Boolean = {
        if (model != last) {
          last = model
          if (model.companion.Key.hashCode() == condition.companion.Key.hashCode()) {
            satisfied = condition.satisfiedBy(model)
          }
        }
        successors(context).activate(resultSoFar && satisfied, context, model, set)
      }
    }

    case class Terminal(ruleName: String) extends Node {
      def activate(resultSoFar: Boolean, context: RuleContext, model: BaseModel, set: ConflictSet): Boolean =
        if (resultSoFar && ruleName == context._1) {
          set += rules(ruleName)
          true
        } else false
    }
  }

  private[this] val network = mutable.AnyRefMap.empty[String, Seq[(UUID, Node.Alpha)]]

  private[this] val rules = mutable.AnyRefMap.empty[String, Rule]

  def compile(from: Iterator[Rule]): Unit = {
    network.clear()
    rules.clear()

    val alphas = mutable.AnyRefMap.empty[Trigger.Condition[_], Node.Alpha].withDefault(cond =>
      new Node.Alpha(cond.asInstanceOf[Trigger.Condition[BaseModel]], mutable.AnyRefMap.empty)
    )

    @tailrec def rec(ctx: RuleContext, terminal: Node.Terminal,
      current: Node.Alpha, tail: Seq[Trigger.Condition[_]]): Unit = {
      if (tail.isEmpty) {
        current.successors += (ctx, terminal)
      } else {
        val next = alphas(tail.head)
        if (!next.successors.contains(ctx)) {
          alphas += (next.condition -> next)
          current.successors +=(ctx, next)
          rec(ctx, terminal, next, tail.tail)
        } else rec(ctx, terminal, current, tail.tail)
      }
    }

    for(rule <- from) {
      lazy val terminal = new Node.Terminal(rule.name)
      val inner = Seq.newBuilder[(UUID, Node.Alpha)]
      for(branch <- rule.direct) {
        val ctx = (rule.name, branch.uuid)
        val alpha = alphas(branch.conditions.head)
        alphas += (alpha.condition -> alpha)
        inner += (branch.uuid -> alpha)

        rec(ctx, terminal, alpha, branch.conditions.tail)
      }
      network += (rule.name, inner.result())
      rules += (rule.name, rule)
    }

    network.repack()
    rules.repack()
  }

  def activate(model: BaseModel): TraversableOnce[Rule] = {
    val conflictSet = mutable.Set.empty[Rule]
    network.foreach { branches =>
      lazy val rule = rules(branches._1)
      var satisfied = false
      branches._2.foreach { kv =>
        val newSatisfied = kv._2.activate(true, (branches._1, kv._1), model, conflictSet)
        satisfied = satisfied || newSatisfied
      }
      if (!satisfied && rule.executed) rule.executed = false
    }
    conflictSet
  }

  def feed(initial: TraversableOnce[BaseModel]): TraversableOnce[Rule] =
    initial.map(activate).toVector.last
}

object ExecutionNetwork {
  def apply(from: Iterator[Rule]) = {
    val network = new ExecutionNetwork
    network.compile(from)
    network
  }

  def apply() = new ExecutionNetwork
}
