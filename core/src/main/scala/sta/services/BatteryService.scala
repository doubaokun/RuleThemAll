package sta.services

import android.content.Intent
import android.os.BatteryManager
import kj.android.common.Option
import spire.math.UByte
import sta.model.system._

import scala.concurrent.duration._

@genReactOn
@manual(Intent.ACTION_BATTERY_CHANGED -> 1.second)
abstract class BatteryService extends ServiceFragment[Battery] {
  final def handle(intent: Intent) = intent.getAction match {
    case Intent.ACTION_POWER_CONNECTED    ⇒ PowerState.Connected
    case Intent.ACTION_POWER_DISCONNECTED ⇒ PowerState.Disconnected
    case Intent.ACTION_BATTERY_LOW        ⇒ BatteryState.Low
    case Intent.ACTION_BATTERY_OKAY       ⇒ BatteryState.OK
    case Intent.ACTION_BATTERY_CHANGED ⇒
      val status: Option[BatteryStatus] = intent.extra[Int].status.map {
        case BatteryManager.BATTERY_STATUS_CHARGING     ⇒ BatteryStatus.Charging
        case BatteryManager.BATTERY_STATUS_DISCHARGING  ⇒ BatteryStatus.Discharging
        case BatteryManager.BATTERY_STATUS_FULL         ⇒ BatteryStatus.Full
        case BatteryManager.BATTERY_STATUS_NOT_CHARGING ⇒ BatteryStatus.NotCharging
        case BatteryManager.BATTERY_STATUS_UNKNOWN      ⇒ BatteryStatus.Unknown
      }
      val health: Option[BatteryHealth] = intent.extra[Int].health.map {
        case BatteryManager.BATTERY_HEALTH_COLD                ⇒ BatteryHealth.Cold
        case BatteryManager.BATTERY_HEALTH_DEAD                ⇒ BatteryHealth.Dead
        case BatteryManager.BATTERY_HEALTH_GOOD                ⇒ BatteryHealth.Good
        case BatteryManager.BATTERY_HEALTH_OVERHEAT            ⇒ BatteryHealth.Overheat
        case BatteryManager.BATTERY_HEALTH_OVER_VOLTAGE        ⇒ BatteryHealth.OverVoltage
        case BatteryManager.BATTERY_HEALTH_UNKNOWN             ⇒ BatteryHealth.Unknown
        case BatteryManager.BATTERY_HEALTH_UNSPECIFIED_FAILURE ⇒ BatteryHealth.UnspecifiedFailure
      }
      val present: Option[BatteryPresent] = intent.extra[Boolean].present.map(BatteryPresent(_))
      val level: Option[BatteryLevel] = for {
        l ← intent.extra[Int].level
        s ← intent.extra[Int].scale
      } yield BatteryLevel(UByte((l * 100 / s.toDouble).round.toByte))
      val `icon-small`: Option[BatteryIconSmall] = intent.extra[Int].`icon-small`.map(BatteryIconSmall(_))
      val plugged: Option[BatteryPlugged] = intent.extra[Int].plugged.map {
        case BatteryManager.BATTERY_PLUGGED_AC       ⇒ BatteryPlugged.AC
        case BatteryManager.BATTERY_PLUGGED_USB      ⇒ BatteryPlugged.USB
        case BatteryManager.BATTERY_PLUGGED_WIRELESS ⇒ BatteryPlugged.Wireless
      }
      val voltage: Option[BatteryVoltage] = intent.extra[Int].voltage.map(BatteryVoltage(_))
      val temperature: Option[BatteryTemperature] = intent.extra[Int].temperature.map(BatteryTemperature(_))
      val technology: Option[BatteryTechnology] = intent.extra[String].technology.map(BatteryTechnology(_))
      val invalidCharger: Option[BatteryInvalidCharger] = intent.extra[Int].invalid_charger.map(ic ⇒ BatteryInvalidCharger(ic != 0))

      val result: List[Option[Battery]] =
        List(status, health, present, level, `icon-small`, plugged, voltage, temperature, technology, invalidCharger)
      result.flatten
  }
}
