package sta.parser.triggers

import kj.android.common.UsedFeatures
import org.parboiled2._
import spire.math.UByte
import sta.model.system._
import sta.model.triggers._
import sta.model.triggers.functions._
import kj.android.common.UsedFeatures._

import scala.language.implicitConversions
import scalaz._

trait BatteryRules extends TriggerParserPart { this: TriggerParser ⇒
  protected def prefix: String = implicitly[UsedFeatures[Battery]].category

  private implicit def bt(l: BatteryLevel): UByte = l.value
  private implicit def eq: Equal[BatteryPlugged] = Equal.equalA

  private def level: Rule1[AtomicTrigger[BatteryLevel]] = rule(
    "level" ~ "is" ~ (
      (("below" ~ Percent) ~> (n ⇒ AtomicTrigger[BatteryLevel](LTFunction(n)))) |
      (("above" ~ Percent) ~> (n ⇒ AtomicTrigger[BatteryLevel](GTFunction(n))))
    )
  )

  private def plugged: Rule1[AtomicTrigger[BatteryPlugged]] = rule(
    "plugged" ~ (
      (endWS("ac") ~ push(AtomicTrigger[BatteryPlugged](EqualFunction(BatteryPlugged.AC)))) |
      (endWS("usb") ~ push(AtomicTrigger[BatteryPlugged](EqualFunction(BatteryPlugged.USB)))) |
      (endWS("wireless") ~ push(AtomicTrigger[BatteryPlugged](EqualFunction(BatteryPlugged.Wireless))))
    )
  )

  protected def MainT: Rule1[Trigger] = rule(level | plugged)
}
