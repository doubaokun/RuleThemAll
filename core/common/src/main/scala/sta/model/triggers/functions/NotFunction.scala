package sta.model.triggers.functions

import sta.model.Model

case class NotFunction[M <: Model](fun: ModelFunction[M]) extends ModelFunction[M] {
  def apply(m: M): Boolean = !fun(m)

  override def toString() = s"!($fun)"
}
