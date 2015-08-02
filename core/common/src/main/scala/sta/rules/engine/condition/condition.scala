//package sta.rules.engine.condition
//
//import scala.reflect.runtime.{ universe => ru }
//import spire.algebra._
//import spire.syntax.order._
//import sta.rules.engine.utils.Hash
//
//abstract class Condition[T: ru.TypeTag] extends (T => Boolean) {
//  def conforms(tpe: ru.Type): Boolean = tpe <:< ru.typeOf[T]
//}
//
//abstract class HashBasedCondition[T] extends Condition[T] {
//  @inline def hash: Long
//}
//
//object Condition {
//
//  case class Equal[T: ru.TypeTag: Order](value: T) extends HashBasedCondition[T] {
//    @inline def hash: Long = value.hashCode()
//
//    @inline def apply(other: T): Boolean = value === other
//
//    override def toString(): String = s"x == $value"
//  }
//
//  case class NotEqual[T: ru.TypeTag: Order](value: T) extends Condition[T] {
//    @inline def apply(other: T): Boolean = value =!= other
//
//    override def toString(): String = s"x != $value"
//  }
//
//  case class Negate[T: ru.TypeTag](condition: Condition[T]) extends Condition[T] {
//    @inline def apply(value: T): Boolean = !condition.apply(value)
//
//    override def toString(): String = s"!(${condition.toString()}})"
//  }
//
//  object AcceptAll extends HashBasedCondition[Any] {
//    @inline def hash: Long = Hash.neutral
//
//    @inline def apply(v: Any): Boolean = true
//
//    override def toString(): String = "true"
//  }
//
//  case class GreaterThan[T: ru.TypeTag: Order](value: T) extends Condition[T] {
//    @inline def apply(other: T): Boolean = value < other
//
//    override def toString(): String = s"x > $value"
//  }
//
//  case class GreaterThanOrEqual[T: ru.TypeTag: Order](value: T) extends Condition[T] {
//    @inline def apply(other: T): Boolean = value <= other
//
//    override def toString(): String = s"x >= $value"
//  }
//
//  case class LessThan[T: ru.TypeTag: Order](v1: T) extends Condition[T] {
//    @inline def apply(v2: T): Boolean = v1 > v2
//
//    override def toString(): String = s"x < $v1"
//  }
//
//  case class LessThanOrEqual[T: ru.TypeTag: Order](value: T) extends Condition[T] {
//    @inline def apply(other: T): Boolean = value >= other
//
//    override def toString(): String = s"x <= $value"
//  }
//
//}
