package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Trigger
import sta.model.triggers.Implicits._

object BatteryRules extends TriggerParser[BatteryLike] {
  import Battery._
  import white._

  def Prefix: String = Uses.categoryOf[BatteryLike]

  private def powerState: P[Trigger.Condition[PowerState]] = {
    "power" ~ mapParser(PowerState.namesToValuesMap) map (v =>
      Trigger.Condition[PowerState](_ == v))
  }

  private def batteryState: P[Trigger.Condition[BatteryState]] = {
    "state" ~ mapParser(BatteryState.namesToValuesMap) map (v => Trigger.Condition[BatteryState](_ == v))
  }

  private def level: P[Trigger.Condition[Battery]] = {
    "level" ~ (
      (("<" ~ Percent) map (n => Trigger.Condition[Battery](_.level < n))) |
      (("<=" ~ Percent) map (n => Trigger.Condition[Battery](_.level <= n))) |
      ((">" ~ Percent) map (n => Trigger.Condition[Battery](_.level > n))) |
      ((">=" ~ Percent) map (n => Trigger.Condition[Battery](_.level >= n))) |
      (("==" ~ Percent) map (n => Trigger.Condition[Battery](_.level == n))) |
      (("!=" ~ Percent) map (n => Trigger.Condition[Battery](_.level != n)))
    )
  }

  private def plugged: P[Trigger.Condition[Battery]] = {
    "plugged" ~ mapParser(Plugged.namesToValuesMap) map (v => Trigger.Condition[Battery](_.plugged == v))
  }

  private def present: P[Trigger.Condition[Battery]] = {
    ("present".u map (_ => Trigger.Condition[Battery](_.present == true))) |
      ("absent".u map (_ => Trigger.Condition[Battery](_.present == false)))
  }

  private def status: P[Trigger.Condition[Battery]] = {
    "status" ~ mapParser(Status.namesToValuesMap) map (v => Trigger.Condition[Battery](_.status == v))
  }

  def Main: P[Trigger.Standalone[_ <: BatteryLike]] =
    powerState | batteryState | level | plugged | present | status
}
