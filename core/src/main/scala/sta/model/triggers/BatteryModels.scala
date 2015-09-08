package sta.model.triggers

import android.content.Intent
import android.os.BatteryManager
import enumeratum.Enum
import spire.math.UByte
import sta.common.{category, action}
import sta.model._

trait BatteryModels {

  @category("battery")
  sealed abstract class BatteryLike(companion: ModelCompanion[BatteryLike]) extends Model[BatteryLike](companion)

  sealed abstract class PowerState extends BatteryLike(PowerState) with ModelEnumEntry
  implicit object PowerState extends ModelCompanion[PowerState] with Enum[PowerState] {
    lazy val values = findValues

    @action(Intent.ACTION_POWER_CONNECTED) case object Connected extends PowerState
    @action(Intent.ACTION_POWER_DISCONNECTED) case object Disconnected extends PowerState
  }

  sealed abstract class BatteryState extends BatteryLike(BatteryState) with ModelEnumEntry
  implicit object BatteryState extends ModelCompanion[BatteryState] with Enum[BatteryState] {
    lazy val values = findValues

    @action(Intent.ACTION_BATTERY_LOW) case object Low extends BatteryState
    @action(Intent.ACTION_BATTERY_OKAY) case object OK extends BatteryState
  }

  @action(Intent.ACTION_BATTERY_CHANGED)
  case class Battery(level: UByte, plugged: Battery.Plugged,
                     present: Boolean, status: Battery.Status) extends BatteryLike(Battery)
  implicit object Battery extends ModelCompanion[Battery] {
    sealed abstract class Plugged extends FromIntEntry
    object Plugged extends Enum[Plugged] with FromInt[Plugged] {
      lazy val values = findValues

      case object AC extends Plugged {
        def intValue: Int = BatteryManager.BATTERY_PLUGGED_AC
      }
      case object USB extends Plugged {
        def intValue: Int = BatteryManager.BATTERY_PLUGGED_USB
      }
      case object Wireless extends Plugged {
        def intValue: Int = BatteryManager.BATTERY_PLUGGED_WIRELESS
      }
    }

    sealed abstract class Status extends FromIntEntry
    object Status extends Enum[Status] with FromInt[Status] {
      lazy val values = findValues

      case object Charging extends Status {
        def intValue: Int = BatteryManager.BATTERY_STATUS_CHARGING
      }
      case object Discharging extends Status {
        def intValue: Int = BatteryManager.BATTERY_STATUS_DISCHARGING
      }
      case object Full extends Status {
        def intValue: Int = BatteryManager.BATTERY_STATUS_FULL
      }
      case object NotCharging extends Status {
        def intValue: Int = BatteryManager.BATTERY_STATUS_NOT_CHARGING
      }
      case object Unknown extends Status {
        def intValue: Int = BatteryManager.BATTERY_STATUS_UNKNOWN
      }
    }
  }

}
