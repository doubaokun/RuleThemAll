package sta.parser.actions

import org.parboiled2._
import sta.model.actions.Action
import sta.parser.{ BasicRules, RichParser }

trait ActionParser extends RichParser with BasicRules {
  def MainA: Rule1[Action]
}

trait ActionParserPart { this: ActionParser ⇒
  protected def MainA: Rule1[Action]
}

trait SetActionParserPart extends ActionParserPart { this: ActionParser ⇒
  protected def MainA: Rule1[Action] = rule("set" ~ ruleObject ~ "to" ~ ruleAdverb)

  protected def ruleObject: String

  protected def ruleAdverb: Rule1[Action]
}
