package sta.model.triggers.functions

import sta.model.BaseModel

/** Negates result of the underlying predicate. */
case class NotFunction[M <: BaseModel](fun: ModelFunction[M]) extends ModelFunction[M] {
  def apply(m: M): Boolean = !fun(m)

  override def toString() = s"!($fun)"
}
