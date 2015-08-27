package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.ModelTrigger
import sta.model.triggers.Implicits._

object HeadsetRules extends TriggerParser[Headset] {
  import white._

  def Prefix: String = Uses.categoryOf[Headset]

  private def connectivity: P[ModelTrigger[Headset]] = {
    mapParser(Headset.namesToValuesMap) map (v => ModelTrigger[Headset](_ == v))
  }

  val Rule: P[ModelTrigger[_ <: Headset]] = connectivity
}
