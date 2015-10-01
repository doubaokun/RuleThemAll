package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Implicits._
import sta.model.triggers.Trigger
import sta.parser.TriggerParser

object HeadsetRules extends TriggerParser[Headset] {
  import white._

  def Prefix: String = Uses.categoryOf[Headset]

  private def connectivity: P[Trigger.Condition[Headset]] = {
    mapParser(Headset.namesToValuesMap) map (v => Trigger.Condition[Headset](_ == v))
  }

  def Main: P[Trigger.Standalone[_ <: Headset]] = connectivity
}
