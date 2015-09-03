package sta.parser.triggers

import fastparse.noApi._
import kj.android.concurrent.Task
import sta.model.triggers.Trigger

object TimeRules extends TriggerParser[Nothing] {
  import white._

  def Prefix: String = "time"

  private def time = "matches" ~ CronExpression map Trigger.Timer.apply

  def Main: P[Trigger.Standalone[_ <: Nothing]] = time
}
