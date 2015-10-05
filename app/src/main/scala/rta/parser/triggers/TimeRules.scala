package rta.parser.triggers

import fastparse.noApi._
import rta.model.triggers.Trigger
import rta.parser.TriggerParser

object TimeRules extends TriggerParser[Nothing] {
  import white._

  def Prefix: String = "time"

  private def time = "matches".withWS ~ CronExpr map Trigger.Timer.apply

  def Main: P[Trigger.Standalone[_ <: Nothing]] = time
}
