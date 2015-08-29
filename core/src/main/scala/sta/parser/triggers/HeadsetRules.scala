package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Trigger
import sta.model.triggers.Implicits._

object HeadsetRules extends TriggerParser[Headset] {
  import white._

  def Prefix: String = Uses.categoryOf[Headset]

  private def connectivity: P[Trigger.Atomic[Headset]] = {
    mapParser(Headset.namesToValuesMap) map (v => Trigger.Atomic[Headset](_ == v))
  }

  val Rule: P[Trigger.Atomic[_ <: Headset]] = connectivity
}
