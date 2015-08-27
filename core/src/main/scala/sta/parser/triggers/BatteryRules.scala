package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.ModelTrigger
import sta.model.triggers.Implicits._

object BatteryRules extends TriggerParser[BatteryLike] {
  import Battery._
  import white._

  def Prefix: String = Uses.categoryOf[BatteryLike]

  private def powerState: P[ModelTrigger[PowerState]] = {
    "power" ~ mapParser(PowerState.namesToValuesMap) map (v =>
      ModelTrigger[PowerState](_ == v))
  }

  private def batteryState: P[ModelTrigger[BatteryState]] = {
    "state" ~ mapParser(BatteryState.namesToValuesMap) map (v => ModelTrigger[BatteryState](_ == v))
  }

  private def level: P[ModelTrigger[Battery]] = {
    "level" ~ (
      (("<" ~ Percent) map (n => ModelTrigger[Battery](_.level < n))) |
      (("<=" ~ Percent) map (n => ModelTrigger[Battery](_.level <= n))) |
      ((">" ~ Percent) map (n => ModelTrigger[Battery](_.level > n))) |
      ((">=" ~ Percent) map (n => ModelTrigger[Battery](_.level >= n))) |
      (("==" ~ Percent) map (n => ModelTrigger[Battery](_.level == n))) |
      (("!=" ~ Percent) map (n => ModelTrigger[Battery](_.level != n)))
    )
  }

  private def plugged: P[ModelTrigger[Battery]] = {
    "plugged" ~ mapParser(Plugged.namesToValuesMap) map (v => ModelTrigger[Battery](_.plugged == v))
  }

  private def present: P[ModelTrigger[Battery]] = {
    ("present".! map (_ => ModelTrigger[Battery](_.present == true))) |
      ("absent".! map (_ => ModelTrigger[Battery](_.present == false)))
  }

  private def status: P[ModelTrigger[Battery]] = {
    "status" ~ mapParser(Status.namesToValuesMap) map (v => ModelTrigger[Battery](_.status == v))
  }

  val Rule: P[ModelTrigger[_ <: BatteryLike]] =
    powerState | batteryState | level | plugged | present | status
}
