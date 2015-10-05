package rta.parser.triggers

import fastparse.noApi._
import rta.common.Uses
import rta.model.triggers.Implicits._
import rta.model.triggers.Trigger
import rta.parser.TriggerParser

object HeadsetRules extends TriggerParser[Headset] {
  import white._

  def Prefix: String = Uses.categoryOf[Headset]

  private def connectivity: P[Trigger.Condition[Headset]] = {
    mapParser(Headset.namesToValuesMap) map (v => Trigger.Condition[Headset](_ == v))
  }

  def Main: P[Trigger.Standalone[_ <: Headset]] = connectivity
}
