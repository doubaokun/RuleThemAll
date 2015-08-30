package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Condition
import sta.model.triggers.Implicits._

object HeadsetRules extends TriggerParser[Headset] {
  import white._

  def Prefix: String = Uses.categoryOf[Headset]

  private def connectivity: P[Condition.Trigger[Headset]] = {
    mapParser(Headset.namesToValuesMap) map (v => Condition.Trigger[Headset](_ == v))
  }

  val Rule: P[Condition.Standalone[_ <: Headset]] = connectivity
}
