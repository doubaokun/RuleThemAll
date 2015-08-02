package sta.model.triggers.functions

import sta.model.Model

case class NotEqualFunction[V, M <: Model: Has[V]#Conversion](v: V) extends ModelFunction[M] {
  def apply(m: M): Boolean = implicitly[Has[V]#Conversion[M]].apply(m) != v

  override def toString(): String = s"x != $v"
}
