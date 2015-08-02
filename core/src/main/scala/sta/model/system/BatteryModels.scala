package sta.model.system

import android.os.BatteryManager
import enumeratum.{Enum, EnumEntry}
import kj.android.common.category
import spire.math.UByte
import sta.model.{FromInt, Model, ModelCompanion, ModelEnumEntry}

trait BatteryModels {

  @category("battery")
  abstract class BatteryLike(companion: ModelCompanion[BatteryLike]) extends Model(companion)

  sealed abstract class PowerState extends BatteryLike(PowerState) with ModelEnumEntry
  implicit object PowerState extends ModelCompanion[PowerState] with Enum[PowerState] {
    lazy val values = findValues

    case object Connected extends PowerState
    case object Disconnected extends PowerState
  }

  sealed abstract class BatteryState extends BatteryLike(BatteryState) with ModelEnumEntry
  implicit object BatteryState extends ModelCompanion[BatteryState] with Enum[BatteryState] {
    lazy val values = findValues

    case object Low extends BatteryState
    case object OK extends BatteryState
  }

  case class Battery(level: UByte, plugged: Battery.Plugged,
                     present: Boolean, status: Battery.Status) extends BatteryLike(Battery)
  implicit object Battery extends ModelCompanion[Battery] {
    sealed abstract class Plugged extends EnumEntry { this: Product =>
      override def entryName: String = productPrefix.toLowerCase
    }
    object Plugged extends Enum[Plugged] with FromInt[Plugged] {
      lazy val intValues = Map(
        BatteryManager.BATTERY_PLUGGED_AC -> Plugged.AC,
        BatteryManager.BATTERY_PLUGGED_USB -> Plugged.USB,
        BatteryManager.BATTERY_PLUGGED_WIRELESS -> Plugged.Wireless
      )

      lazy val values = findValues

      case object AC extends Plugged
      case object USB extends Plugged
      case object Wireless extends Plugged
    }

    sealed abstract class Status extends ModelEnumEntry
    object Status extends Enum[Status] with FromInt[Status] {
      lazy val intValues = Map(
        BatteryManager.BATTERY_STATUS_UNKNOWN -> Status.Unknown,
        BatteryManager.BATTERY_STATUS_CHARGING -> Status.Charging,
        BatteryManager.BATTERY_STATUS_DISCHARGING -> Status.Discharging,
        BatteryManager.BATTERY_STATUS_NOT_CHARGING -> Status.NotCharging,
        BatteryManager.BATTERY_STATUS_FULL -> Status.Full
      )

      lazy val values = findValues

      case object Charging extends Status
      case object Discharging extends Status
      case object Full extends Status
      case object NotCharging extends Status
      case object Unknown extends Status
    }
  }

}
