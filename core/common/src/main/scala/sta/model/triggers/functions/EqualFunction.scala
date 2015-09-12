package sta.model.triggers.functions

import sta.model.BaseModel

/** Returns true if part of the model is equal to the memoized value. */
case class EqualFunction[V, M <: BaseModel: Has[V]#Conversion](v: V) extends HashBasedFunction[V, M] {
  def apply(m: M): Boolean = implicitly[Has[V]#Conversion[M]].apply(m) == v

  override def toString(): String = s"x == $v"
}

