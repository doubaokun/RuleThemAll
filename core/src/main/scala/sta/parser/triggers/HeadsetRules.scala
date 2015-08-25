package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.AtomicTrigger
import sta.model.triggers.Implicits._

object HeadsetRules extends TriggerParser[Headset] {
  import white._

  def Prefix: String = Uses.categoryOf[Headset]

  private def connectivity: P[AtomicTrigger[Headset]] = {
    mapParser(Headset.namesToValuesMap) map (v => AtomicTrigger[Headset](_ == v))
  }

  val Rule: P[AtomicTrigger[_ <: Headset]] = connectivity
}
