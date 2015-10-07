package rta.model.triggers.functions

import rta.model.BaseModel

/** Negates result of the underlying predicate. */
final case class NotFunction[M <: BaseModel](fun: ModelFunction[M]) extends ModelFunction[M] {
  def apply(m: M): Boolean = !fun(m)

  def `unary_!`: ModelFunction[M] = fun

  override def toString() = s"!($fun)"
}
