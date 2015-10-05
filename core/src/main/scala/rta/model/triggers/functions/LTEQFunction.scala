package rta.model.triggers.functions

import spire.algebra._
import spire.syntax.order._
import rta.model.BaseModel

/** Returns true if part of the model is less than or equal to the memoized value. */
final case class LTEQFunction[V: Order, M <: BaseModel: Has[V]#Conversion](v: V) extends ModelFunction[M] {
  def apply(m: M): Boolean = v >= m

  override def toString(): String = s"x <= $v"
}
