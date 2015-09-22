package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Trigger
import sta.model.triggers.Implicits._
import sta.parser.TriggerParser

object BatteryRules extends TriggerParser[BatteryLike] {
  import Battery._
  import white._

  def Prefix: String = Uses.categoryOf[BatteryLike]

  private def powerState: P[Trigger.Condition[PowerState]] = {
    "power".withWS ~ mapParser(PowerState.namesToValuesMap).map(v =>
      Trigger.Condition[PowerState](_ == v))
  }

  private def batteryState: P[Trigger.Condition[BatteryState]] = {
    "state".withWS ~ mapParser(BatteryState.namesToValuesMap).map(v => Trigger.Condition[BatteryState](_ == v))
  }

  private def level: P[Trigger.Condition[Battery]] = {
    "level" ~ (
      ("<" ~ Percent).map(n => Trigger.Condition[Battery](_.level < n)) |
      ("<=" ~ Percent).map(n => Trigger.Condition[Battery](_.level <= n)) |
      (">" ~ Percent).map(n => Trigger.Condition[Battery](_.level > n)) |
      (">=" ~ Percent).map(n => Trigger.Condition[Battery](_.level >= n)) |
      ("==" ~ Percent).map(n => Trigger.Condition[Battery](_.level == n)) |
      ("!=" ~ Percent).map(n => Trigger.Condition[Battery](_.level != n))
    )
  }

  private def plugged: P[Trigger.Condition[Battery]] = {
    "plugged".withWS ~ mapParser(Plugged.namesToValuesMap).map(v => Trigger.Condition[Battery](_.plugged == v))
  }

  private def present: P[Trigger.Condition[Battery]] = {
    "present".push(Trigger.Condition[Battery](_.present == true)) |
      "absent".push(Trigger.Condition[Battery](_.present == false))
  }

  private def status: P[Trigger.Condition[Battery]] = {
    "status".withWS ~ mapParser(Status.namesToValuesMap).map(v => Trigger.Condition[Battery](_.status == v))
  }

  def Main: P[Trigger.Standalone[_ <: BatteryLike]] =
    powerState | batteryState | level | plugged | present | status
}
