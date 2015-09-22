package sta.parser

import fastparse.noApi._
import sta.model.actions.Action

abstract class ActionParser[A <: Action] extends BasicRules with Extras with Serializable {
  def Rule: P[A]
}
