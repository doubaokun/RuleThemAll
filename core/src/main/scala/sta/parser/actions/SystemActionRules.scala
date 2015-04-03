package sta.parser.actions

import org.parboiled2.Rule1
import sta.model.actions.Action
import sta.parser.RuleMacros

trait SystemActionRules extends ActionParser
    with SoundProfileRules {
  override final def MainA: Rule1[Action] = RuleMacros.MainA
}
