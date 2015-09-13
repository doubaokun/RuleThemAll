package sta.parser.triggers

import fastparse.noApi._
import sta.model.triggers.Trigger
import sta.parser.TriggerParser

object TimeRules extends TriggerParser[Nothing] {
  import white._

  def Prefix: String = "time"

  private def time = "matches" ~ CronExpression map Trigger.Timer.apply

  def Main: P[Trigger.Standalone[_ <: Nothing]] = time
}
