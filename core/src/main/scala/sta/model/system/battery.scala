package sta.model.system

import kj.android.common.category
import spire.math.UByte
import sta.model.{ Model, ModelCompanion }

trait BatteryModels {
  @category("battery") abstract class Battery(companion: ModelCompanion[Battery]) extends Model(companion)

  sealed abstract class PowerState extends Battery(PowerState)
  implicit object PowerState extends ModelCompanion[PowerState] {
    case object Connected extends PowerState
    case object Disconnected extends PowerState
  }

  sealed abstract class BatteryState extends Battery(BatteryState)
  implicit object BatteryState extends ModelCompanion[BatteryState] {
    case object Low extends BatteryState
    case object OK extends BatteryState
  }

  sealed abstract class BatteryStatus extends Battery(BatteryStatus)
  implicit object BatteryStatus extends ModelCompanion[BatteryStatus] {
    case object Charging extends BatteryStatus
    case object Discharging extends BatteryStatus
    case object Full extends BatteryStatus
    case object NotCharging extends BatteryStatus
    case object Unknown extends BatteryStatus
  }

  sealed abstract class BatteryHealth extends Battery(BatteryHealth)
  implicit object BatteryHealth extends ModelCompanion[BatteryHealth] {
    case object Cold extends BatteryHealth
    case object Dead extends BatteryHealth
    case object Good extends BatteryHealth
    case object Overheat extends BatteryHealth
    case object OverVoltage extends BatteryHealth
    case object Unknown extends BatteryHealth
    case object UnspecifiedFailure extends BatteryHealth
  }

  case class BatteryPresent(value: Boolean) extends Battery(BatteryPresent)
  implicit object BatteryPresent extends ModelCompanion[BatteryPresent]

  case class BatteryLevel(value: UByte) extends Battery(BatteryLevel)
  implicit object BatteryLevel extends ModelCompanion[BatteryLevel]

  case class BatteryIconSmall(value: Int) extends Battery(BatteryIconSmall)
  implicit object BatteryIconSmall extends ModelCompanion[BatteryIconSmall]

  sealed abstract class BatteryPlugged extends Battery(BatteryPlugged)
  implicit object BatteryPlugged extends ModelCompanion[BatteryPlugged] {
    case object AC extends BatteryPlugged
    case object USB extends BatteryPlugged
    case object Wireless extends BatteryPlugged
  }

  case class BatteryVoltage(value: Int) extends Battery(BatteryVoltage)
  implicit object BatteryVoltage extends ModelCompanion[BatteryVoltage]

  case class BatteryTemperature(value: Int) extends Battery(BatteryTemperature)
  implicit object BatteryTemperature extends ModelCompanion[BatteryTemperature]

  case class BatteryTechnology(value: String) extends Battery(BatteryTechnology)
  implicit object BatteryTechnology extends ModelCompanion[BatteryTechnology]

  case class BatteryInvalidCharger(value: Boolean) extends Battery(BatteryInvalidCharger)
  implicit object BatteryInvalidCharger extends ModelCompanion[BatteryInvalidCharger]
}
