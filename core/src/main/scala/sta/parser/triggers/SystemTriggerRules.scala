package sta.parser.triggers

import org.parboiled2.Rule1
import sta.model.triggers.Trigger
import sta.parser.RuleMacros

import scala.annotation.compileTimeOnly

trait SystemTriggerRules extends TriggerParser
    with BatteryRules
    with HeadsetRules {
  @compileTimeOnly("phantom method")
  override final protected def prefix: String = ???

  override final def MainT: Rule1[Trigger] = RuleMacros.MainT
}
