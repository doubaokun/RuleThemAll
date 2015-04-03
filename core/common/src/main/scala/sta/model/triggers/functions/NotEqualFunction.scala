package sta.model.triggers.functions

import sta.model.Model

import scalaz._, Scalaz._

case class NotEqualFunction[M <: Model: Equal](v: M) extends ModelFunction[M] {
  def apply(m: M): Boolean = v =/= m

  override def toString(): String = s"x != $v"
}
