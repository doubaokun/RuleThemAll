package rta.model.triggers.functions

import spire.algebra._
import spire.syntax.order._
import rta.model.BaseModel

/** Returns true if part of the model is less than the memoized value. */
final case class LTFunction[V: Order, M <: BaseModel: Has[V]#Conversion](v: V) extends ModelFunction[M] {
  def apply(m: M): Boolean = v > m

  def `unary_!`: ModelFunction[M] = GTEQFunction(v)

  override def toString(): String = s"x < $v"
}
