//package sta.rules.engine
//
//import scala.annotation.tailrec
//import scala.collection.mutable
//import scala.reflect.runtime.{universe => ru}
//import sta.rules.engine.condition.{HashBasedCondition, Condition}
//
//
//sealed trait AlphaMemoryListener {
//  def rightActivate(wme: Any)
//}
//
//sealed abstract class AlphaMemory[T <: Condition[_]] {
//  private val listeners = mutable.ArrayBuffer.empty[AlphaMemoryListener]
//  private val memory = mutable.Set.empty[Any]
//
//  def +(wme: Any): this.type = {
//    memory += wme
//    for (listener <- listeners) {
//      listener.rightActivate(wme)
//    }
//
//    this
//  }
//
//  def -(wme: Any): this.type = {
//    memory -= wme
//
//    this
//  }
//
//  def items: Iterator[Any] = memory.iterator
//
//  def condition: T
//}
//
//object AlphaMemory {
//  case class HashBased(condition: HashBasedCondition[_]) extends AlphaMemory[HashBasedCondition[_]] {
//    @inline def hash: Long = condition.hash
//  }
//
//  case class BooleanBased(condition: Condition[_]) extends AlphaMemory[Condition] {
//    @inline def matches[T: ru.TypeTag](v: T): Boolean = {
//      if (condition.conforms(ru.typeOf[T])) condition.apply(v.asInstanceOf[_])
//      else false
//    }
//  }
//}
//
//sealed abstract class BetaNode extends Node {
//  type P <: BetaNode
//
//  def parent: P
//}
//
//sealed abstract class StateNode extends BetaNode {
//  def leftActivate(token: Token, wme: WME): Unit
//}
//
//case class Token(wme: WME, parent: Token) {
//  @tailrec
//  final def ancestor(index: Int): WME = {
//    if (index == 0) wme
//    else parent.ancestor(index - 1)
//  }
//}
//
//case class BetaMemory(parent: BetaNode, successors: mutable.Seq[JoinNode], tokens: mutable.Seq[Token]) extends StateNode {
//  type S = JoinNode
//
//  def leftActivate(token: Token, wme: WME): Unit = {
//    val newToken = Token(wme, token)
//    tokens +:=  newToken
//    for (child <- successors) {
//      child.leftActivate(newToken)
//    }
//  }
//}
//
//case class JoinNodeTest(test: Condition[_], arg1: WME => Any, back: Int, arg2: WME => Any)
//  extends ((Token, WME) => Boolean) {
//  def apply(t: Token, w: WME): Boolean = {
//    test.equals(arg1(w)) && test.equals(arg2(t.ancestor(back)))
//  }
//}
//
//case class JoinNode(parent: BetaMemory, successors: mutable.Seq[StateNode],
//                    memory: AlphaMemory[_], tests: mutable.Seq[JoinNodeTest]) extends BetaNode {
//  type P = BetaMemory
//  type S = StateNode
//
//  def leftActivate(token: Token): Unit = {
//    for (
//      wme <- memory.items if tests.forall(_(token, wme));
//      child <- successors
//    ) {
//      child.leftActivate(token, wme)
//    }
//  }
//
//  def rightActivate(wme: WME): Unit = {
//    for (
//      token <- parent.tokens if tests.forall(_(token, wme));
//      child <- successors
//    ) {
//      child.leftActivate(token, wme)
//    }
//  }
//}
//
//case class ProductionNode(parent: BetaNode, successors: Seq[BetaNode]) extends StateNode {
//  def leftActivate(token: Token, wme: WME): Unit = {
//    ???
//  }
//}
