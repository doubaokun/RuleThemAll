package sta.model.triggers.functions

import spire.algebra._
import spire.syntax.order._
import sta.model.BaseModel

/** Returns true if part of the model is greater than the memoized value. */
final case class GTFunction[V: Order, M <: BaseModel: Has[V]#Conversion](v: V) extends ModelFunction[M] {
  def apply(m: M): Boolean = v < m

  override def toString(): String = s"x > $v"
}
