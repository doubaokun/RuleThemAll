package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.Action
import sta.parser.{BasicRules, WhitespaceSkip}

trait ActionParser[A <: Action] extends BasicRules with WhitespaceSkip {
  def Rule: P[A]
}
