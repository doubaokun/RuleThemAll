package sta.services

import android.content.Intent
import android.os.BatteryManager._
import scala.concurrent.duration._
import spire.math.UByte
import sta.model.triggers.Implicits._

@manual(Intent.ACTION_BATTERY_CHANGED -> 1.minute)
class BatteryService extends ServiceFragment[BatteryLike] {
  import Battery._

  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_POWER_CONNECTED =>
      PowerState.Connected
    case intent if intent.getAction == Intent.ACTION_POWER_DISCONNECTED =>
      PowerState.Disconnected
    case intent if intent.getAction == Intent.ACTION_BATTERY_LOW =>
      BatteryState.Low
    case intent if intent.getAction == Intent.ACTION_BATTERY_OKAY =>
      BatteryState.OK
    case intent if intent.getAction == Intent.ACTION_BATTERY_CHANGED =>
      val level = UByte((intent.extra[Int](EXTRA_LEVEL) * 100d /
        intent.extra[Int](EXTRA_SCALE).toDouble).round.toByte)
      val present = intent.extra[Boolean](EXTRA_PRESENT)
      val plugged = Plugged.fromInt(intent.extra[Int](EXTRA_PLUGGED))
      val status = Status.fromInt(intent.extra[Int](EXTRA_STATUS))

      Battery(level, plugged, present, status)
  }
}
