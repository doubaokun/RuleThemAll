package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Trigger
import sta.model.triggers.Implicits._

object BatteryRules extends TriggerParser[BatteryLike] {
  import Battery._
  import white._

  def Prefix: String = Uses.categoryOf[BatteryLike]

  private def powerState: P[Trigger.Atomic[PowerState]] = {
    "power" ~ mapParser(PowerState.namesToValuesMap) map (v =>
      Trigger.Atomic[PowerState](_ == v))
  }

  private def batteryState: P[Trigger.Atomic[BatteryState]] = {
    "state" ~ mapParser(BatteryState.namesToValuesMap) map (v => Trigger.Atomic[BatteryState](_ == v))
  }

  private def level: P[Trigger.Atomic[Battery]] = {
    "level" ~ (
      (("<" ~ Percent) map (n => Trigger.Atomic[Battery](_.level < n))) |
      (("<=" ~ Percent) map (n => Trigger.Atomic[Battery](_.level <= n))) |
      ((">" ~ Percent) map (n => Trigger.Atomic[Battery](_.level > n))) |
      ((">=" ~ Percent) map (n => Trigger.Atomic[Battery](_.level >= n))) |
      (("==" ~ Percent) map (n => Trigger.Atomic[Battery](_.level == n))) |
      (("!=" ~ Percent) map (n => Trigger.Atomic[Battery](_.level != n)))
    )
  }

  private def plugged: P[Trigger.Atomic[Battery]] = {
    "plugged" ~ mapParser(Plugged.namesToValuesMap) map (v => Trigger.Atomic[Battery](_.plugged == v))
  }

  private def present: P[Trigger.Atomic[Battery]] = {
    ("present".! map (_ => Trigger.Atomic[Battery](_.present == true))) |
      ("absent".! map (_ => Trigger.Atomic[Battery](_.present == false)))
  }

  private def status: P[Trigger.Atomic[Battery]] = {
    "status" ~ mapParser(Status.namesToValuesMap) map (v => Trigger.Atomic[Battery](_.status == v))
  }

  val Rule: P[Trigger.Atomic[_ <: BatteryLike]] =
    powerState | batteryState | level | plugged | present | status
}
