package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Condition
import sta.model.triggers.Implicits._

object BatteryRules extends TriggerParser[BatteryLike] {
  import Battery._
  import white._

  def Prefix: String = Uses.categoryOf[BatteryLike]

  private def powerState: P[Condition.Trigger[PowerState]] = {
    "power" ~ mapParser(PowerState.namesToValuesMap) map (v =>
      Condition.Trigger[PowerState](_ == v))
  }

  private def batteryState: P[Condition.Trigger[BatteryState]] = {
    "state" ~ mapParser(BatteryState.namesToValuesMap) map (v => Condition.Trigger[BatteryState](_ == v))
  }

  private def level: P[Condition.Trigger[Battery]] = {
    "level" ~ (
      (("<" ~ Percent) map (n => Condition.Trigger[Battery](_.level < n))) |
      (("<=" ~ Percent) map (n => Condition.Trigger[Battery](_.level <= n))) |
      ((">" ~ Percent) map (n => Condition.Trigger[Battery](_.level > n))) |
      ((">=" ~ Percent) map (n => Condition.Trigger[Battery](_.level >= n))) |
      (("==" ~ Percent) map (n => Condition.Trigger[Battery](_.level == n))) |
      (("!=" ~ Percent) map (n => Condition.Trigger[Battery](_.level != n)))
    )
  }

  private def plugged: P[Condition.Trigger[Battery]] = {
    "plugged" ~ mapParser(Plugged.namesToValuesMap) map (v => Condition.Trigger[Battery](_.plugged == v))
  }

  private def present: P[Condition.Trigger[Battery]] = {
    ("present".! map (_ => Condition.Trigger[Battery](_.present == true))) |
      ("absent".! map (_ => Condition.Trigger[Battery](_.present == false)))
  }

  private def status: P[Condition.Trigger[Battery]] = {
    "status" ~ mapParser(Status.namesToValuesMap) map (v => Condition.Trigger[Battery](_.status == v))
  }

  val Rule: P[Condition.Standalone[_ <: BatteryLike]] =
    powerState | batteryState | level | plugged | present | status
}
