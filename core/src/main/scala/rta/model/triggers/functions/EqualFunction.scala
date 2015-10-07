package rta.model.triggers.functions

import rta.model.BaseModel

/** Returns true if part of the model is equal to the memoized value. */
final case class EqualFunction[V, M <: BaseModel: Has[V]#Conversion](v: V) extends HashBasedFunction[V, M] {
  def apply(m: M): Boolean = implicitly[Has[V]#Conversion[M]].apply(m) == v

  def `unary_!`: ModelFunction[M] = NotEqualFunction(v)

  override def toString(): String = s"x == $v"
}

