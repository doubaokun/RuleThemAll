package sta.model.triggers.functions

import spire.algebra._
import spire.syntax.order._
import sta.model.Model

case class GTFunction[V: Order, M <: Model: Has[V]#Conversion](v: V) extends ModelFunction[M] {
  def apply(m: M): Boolean = v < m

  override def toString(): String = s"x > $v"
}
