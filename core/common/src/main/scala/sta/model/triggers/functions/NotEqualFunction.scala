package sta.model.triggers.functions

import sta.model.BaseModel

/** Returns true if part of the model is not equal to the memoized value. */
case class NotEqualFunction[V, M <: BaseModel: Has[V]#Conversion](v: V) extends ModelFunction[M] {
  def apply(m: M): Boolean = implicitly[Has[V]#Conversion[M]].apply(m) != v

  override def toString(): String = s"x != $v"
}
