package sta.services

import android.content.Intent
import android.os.BatteryManager._
import kj.android.common.UsedFeatures
import scala.concurrent.duration._
import spire.math.UByte
import sta.model.system.Implicits._

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
      val level = UByte((intent.extra[Int].get(EXTRA_LEVEL) * 100d /
        intent.extra[Int].get(EXTRA_SCALE).toDouble).round.toByte)
      val present = intent.extra[Boolean].get(EXTRA_PRESENT)
      val plugged = Plugged.fromInt(intent.extra[Int].get(EXTRA_PLUGGED))
      val status = Status.fromInt(intent.extra[Int].get(EXTRA_STATUS))

      Battery(level, plugged, present, status)
  }

  def reactOn: Set[String] = implicitly[UsedFeatures[BatteryLike]].intents
}
