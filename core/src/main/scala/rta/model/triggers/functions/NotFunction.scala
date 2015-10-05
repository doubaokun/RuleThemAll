package rta.model.triggers.functions

import rta.model.BaseModel

/** Negates result of the underlying predicate. */
final case class NotFunction[M <: BaseModel](fun: ModelFunction[M]) extends ModelFunction[M] {
  def apply(m: M): Boolean = !fun(m)

  override def toString() = s"!($fun)"
}
