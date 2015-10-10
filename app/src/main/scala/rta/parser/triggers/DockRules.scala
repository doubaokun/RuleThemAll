package rta.parser.triggers

import fastparse.noApi._
import rta.common.Uses
import rta.model.triggers.Implicits._
import rta.model.triggers.Trigger
import rta.parser.TriggerParser

object DockRules extends TriggerParser[DockState] {
  import white._

  def Prefix: String = Uses.categoryOf[DockState]

  private def state = {
    mapParser(DockState.namesToValuesMap).map(v => Trigger.Condition[DockState](_ == v)) |
      ("any" ~ "desk" ~ "connected" push Trigger.Condition[DockState] {
        case dock: DockState.DockType => dock.isDesk
        case _ => false
      }) | ("connected" push Trigger.Condition[DockState](_.isInstanceOf[DockState.DockType]))
  }

  def Main: P[Trigger.Standalone[_ <: DockState]] = state
}
