package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.Action
import sta.parser.{ RulesParser$, WhitespaceSkip, BasicRules }

trait ActionParser extends BasicRules {
  def Main: P[Action]
}

trait SetActionParser extends ActionParser with WhitespaceSkip {
  import white._

  final def Main: P[Action] = P("set" ~ ruleObject ~ "to" ~ ruleAdverb)

  protected def ruleObject: String

  protected def ruleAdverb: P[Action]
}
