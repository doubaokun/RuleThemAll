package sta.model.triggers.functions

import sta.model.Model

case class EqualFunction[V, M <: Model: Has[V]#Conversion](v: V) extends HashBasedFunction[V, M] {
  def apply(m: M): Boolean = implicitly[Has[V]#Conversion[M]].apply(m) == v

  override def toString(): String = s"x == $v"
}

