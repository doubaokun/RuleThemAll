package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.Action
import sta.parser.{BasicRules, WhitespaceSkip}

trait ActionParser[A <: Action] extends BasicRules with WhitespaceSkip {
  def Rule: P[A]
}

trait SetActionParser[A <: Action] extends ActionParser[A] {
  import white._

  lazy val Rule: P[A] = {
    val ruleObjectParser = {
      val splitted = ruleObject.split("\\s+")
      splitted.tail.foldLeft(splitted.head: P[Unit]) { _ ~ _ }
    }
    P("set" ~ ruleObjectParser ~ "to" ~ ruleAdverb)
  }

  protected def ruleObject: String

  protected def ruleAdverb: P[A]
}
