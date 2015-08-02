package sta.services

import android.content.Intent
import scala.concurrent.duration._
import spire.math.UByte
import sta.model.system._
import sta.model.system.Battery._

@genReactOn
@manual(Intent.ACTION_BATTERY_CHANGED -> 1.second)
abstract class BatteryService extends ServiceFragment[BatteryLike] {
  final def handle(intent: Intent) = intent.getAction match {
    case Intent.ACTION_POWER_CONNECTED => PowerState.Connected
    case Intent.ACTION_POWER_DISCONNECTED => PowerState.Disconnected
    case Intent.ACTION_BATTERY_LOW => BatteryState.Low
    case Intent.ACTION_BATTERY_OKAY => BatteryState.OK
    case Intent.ACTION_BATTERY_CHANGED =>
      val level =
        UByte((intent.extra[Int].level * 100d / intent.extra[Int].scale.toDouble).round.toByte)
      val present = intent.extra[Boolean].present
      val plugged = Plugged.fromInt(intent.extra[Int].plugged)
      val status = Status.fromInt(intent.extra[Int].status)

      Battery(level, plugged, present, status)
  }
}
