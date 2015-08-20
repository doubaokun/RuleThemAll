package sta.parser.triggers

import fastparse.noApi._
import sta.common.UsedFeatures
import sta.model.triggers.AtomicTrigger
import sta.model.triggers.Implicits._

object BatteryRules extends TriggerParser[BatteryLike] {
  import Battery._
  import white._

  def Prefix: String = implicitly[UsedFeatures[BatteryLike]].category

  private def powerState: P[AtomicTrigger[PowerState]] = {
    "power" ~ mapParser(PowerState.namesToValuesMap) map (v =>
      AtomicTrigger[PowerState](_ == v))
  }

  private def batteryState: P[AtomicTrigger[BatteryState]] = {
    "state" ~ mapParser(BatteryState.namesToValuesMap) map (v => AtomicTrigger[BatteryState](_ == v))
  }

  private def level: P[AtomicTrigger[Battery]] = {
    "level" ~ (
      (("<" ~ Percent) map (n => AtomicTrigger[Battery](_.level < n))) |
      (("<=" ~ Percent) map (n => AtomicTrigger[Battery](_.level <= n))) |
      ((">" ~ Percent) map (n => AtomicTrigger[Battery](_.level > n))) |
      ((">=" ~ Percent) map (n => AtomicTrigger[Battery](_.level >= n))) |
      (("==" ~ Percent) map (n => AtomicTrigger[Battery](_.level == n))) |
      (("!=" ~ Percent) map (n => AtomicTrigger[Battery](_.level != n)))
    )
  }

  private def plugged: P[AtomicTrigger[Battery]] = {
    "plugged" ~ mapParser(Plugged.namesToValuesMap) map (v => AtomicTrigger[Battery](_.plugged == v))
  }

  private def present: P[AtomicTrigger[Battery]] = {
    ("present".! map (_ => AtomicTrigger[Battery](_.present == true))) |
      ("absent".! map (_ => AtomicTrigger[Battery](_.present == false)))
  }

  private def status: P[AtomicTrigger[Battery]] = {
    "status" ~ mapParser(Status.namesToValuesMap) map (v => AtomicTrigger[Battery](_.status == v))
  }

  val Rule: P[AtomicTrigger[_ <: BatteryLike]] =
    powerState | batteryState | level | plugged | present | status
}
