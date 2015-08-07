package sta.parser.triggers

import fastparse.noApi._
import kj.android.common.UsedFeatures
import sta.model.system._
import sta.model.triggers.AtomicTrigger
import sta.parser.WhitespaceSkip

object BatteryRules extends TriggerParser[BatteryLike] with WhitespaceSkip {
  import Battery._
  import white._

  def prefix: String = implicitly[UsedFeatures[BatteryLike]].category

  private def powerState: P[AtomicTrigger[PowerState]] = P(
    "power" ~ mapParser(PowerState.namesToValuesMap) map (v =>
      AtomicTrigger[PowerState](_ == v))
  )

  private def batteryState: P[AtomicTrigger[BatteryState]] = P(
    "state" ~ mapParser(BatteryState.namesToValuesMap) map (v => AtomicTrigger[BatteryState](_ == v))
  )

  private def level: P[AtomicTrigger[Battery]] = P(
    "level" ~ (
      (("<" ~ Percent) map (n => AtomicTrigger[Battery](_.level < n))) |
      (("<=" ~ Percent) map (n => AtomicTrigger[Battery](_.level <= n))) |
      ((">" ~ Percent) map (n => AtomicTrigger[Battery](_.level > n))) |
      ((">=" ~ Percent) map (n => AtomicTrigger[Battery](_.level >= n))) |
      (("==" ~ Percent) map (n => AtomicTrigger[Battery](_.level == n))) |
      (("!=" ~ Percent) map (n => AtomicTrigger[Battery](_.level != n)))
    )
  )

  private def plugged: P[AtomicTrigger[Battery]] = P(
    "plugged" ~ mapParser(Plugged.namesToValuesMap) map (v => AtomicTrigger[Battery](_.plugged == v))
  )

  private def present: P[AtomicTrigger[Battery]] = P(
    ("present".! map (_ => AtomicTrigger[Battery](_.present == true))) |
      ("absent".! map (_ => AtomicTrigger[Battery](_.present == false)))
  )

  private def status: P[AtomicTrigger[Battery]] = P(
    "status" ~ mapParser(Status.namesToValuesMap) map (v => AtomicTrigger[Battery](_.status == v))
  )

  val Main: P[AtomicTrigger[_ <: BatteryLike]] =
    powerState | batteryState | level | plugged | present | status
}
