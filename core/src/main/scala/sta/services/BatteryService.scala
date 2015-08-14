package sta.services

import android.content.Intent
import android.os.BatteryManager._
import scala.concurrent.duration._
import spire.math.UByte
import sta.model.system._
import sta.model.system.Battery._

@manual(Intent.ACTION_BATTERY_CHANGED -> 1.minute)
class BatteryService extends ServiceFragment[BatteryLike] {
  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_POWER_CONNECTED =>
      PowerState.Connected
    case intent if intent.getAction == Intent.ACTION_POWER_DISCONNECTED =>
      PowerState.Disconnected
    case intent if intent.getAction == Intent.ACTION_BATTERY_CHANGED =>
      val level = UByte((intent.extra[Int].get(EXTRA_LEVEL) * 100d /
        intent.extra[Int].get(EXTRA_SCALE).toDouble).round.toByte)
      val present = intent.extra[Boolean].get(EXTRA_PRESENT)
      val plugged = Plugged.fromInt(intent.extra[Int].get(EXTRA_PLUGGED))
      val status = Status.fromInt(intent.extra[Int].get(EXTRA_STATUS))

      Battery(level, plugged, present, status)
  }

  protected[sta] def reactOn: Set[String] = Set(
    Intent.ACTION_POWER_CONNECTED,
    Intent.ACTION_POWER_DISCONNECTED,
    Intent.ACTION_BATTERY_CHANGED
  )
}
