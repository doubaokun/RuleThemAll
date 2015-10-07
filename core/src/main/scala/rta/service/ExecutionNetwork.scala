package rta.service

import rta.model.triggers.Trigger
import rta.model.{BaseModel, Rule}
import scala.annotation.tailrec
import scala.collection.mutable

@SuppressWarnings(Array(
  "org.brianmckenna.wartremover.warts.MutableDataStructures",
  "org.brianmckenna.wartremover.warts.Var"
))
sealed class ExecutionNetwork {
  type ConflictSet = mutable.Set[Rule]

  sealed trait Node {
    def activate(set: ConflictSet, wasSatisfied: Boolean, model: BaseModel): Unit
  }

  private[this] object Node {
    case class Alpha(condition: Trigger.Condition[BaseModel], successors: mutable.AnyRefMap[Node]) extends Node {
      private[this] var satisfied = false

      override def equals(o: Any): Boolean = o match {
        case other: Alpha => condition.equals(other.condition)
        case _ => false
      }

      override def hashCode(): Int = condition.hashCode()

      def activate(set: ConflictSet, wasSatisfied: Boolean, model: BaseModel): Unit = {
        if (model.companion.Key.hashCode() == condition.key.hashCode()) {
          satisfied = condition.satisfiedBy(model)
        }
        successors.foreach(_.activate(set, wasSatisfied && satisfied, model))
      }
    }

    case class Terminal(ruleName: String) extends Node {
      def activate(set: ConflictSet, wasSatisfied: Boolean, model: BaseModel): Unit = if (wasSatisfied) {
        set += rules(ruleName)
      }
    }
  }

  private[this] val network = mutable.Set.empty[Node.Alpha]

  private[this] val rules = mutable.AnyRefMap.empty[String, Rule]

  private def compile(from: Iterator[Rule]): Unit = {
    network.clear()
    rules.clear()

    val alphas = mutable.LongMap.empty[Node.Alpha]

    @tailrec def rec(terminal: Node.Terminal, current: Trigger.Condition[_], tail: Seq[Trigger.Condition[_]]): Unit = {
      @inline def add(node: Node): Unit = {
        val hash = current.hashCode().toLong
        if (alphas.contains(hash)) {
          val alpha = alphas(hash)
//          println(s"Modifying current alpha node $alpha with hash $hash")
          alpha.successors += node
        } else {
//          println(s"Adding new alpha node $current with hash $hash")
          val alpha = new Node.Alpha(current.asInstanceOf[Trigger.Condition[BaseModel]], mutable.Set(node))
          alphas += (hash, alpha)
        }
      }

      if (tail.isEmpty) {
//        println(s"Adding terminal ${terminal.ruleName} for $current")
        add(terminal)
      } else {
        val next = tail.head
//        println(s"Adding successor $next for $current")
        add(alphas.getOrElse(next.hashCode().toLong,
          new Node.Alpha(next.asInstanceOf[Trigger.Condition[BaseModel]], mutable.Set.empty)))
        rec(terminal, next, tail.tail)
      }
    }

    for(rule <- from) {
      for(branch <- rule.direct) {
        val first = branch.conditions.head
        rec(new Node.Terminal(rule.name), first, branch.conditions.tail)
        network += alphas(first.hashCode().toLong)
      }
      rules += (rule.name, rule)
    }

    rules.repack()
  }

  def activate(model: BaseModel): ConflictSet = {
    val conflictSet = mutable.Set.empty[Rule]
    network.foreach(_.activate(conflictSet, true, model))
    conflictSet
  }
}

object ExecutionNetwork {
  def apply(from: Iterator[Rule]) = {
    val network = new ExecutionNetwork
    network.compile(from)
    network
  }
}
