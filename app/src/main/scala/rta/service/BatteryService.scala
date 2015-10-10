package rta.service

import android.content.Intent
import android.os.BatteryManager._
import rta.common.Uses
import scala.concurrent.duration._
import spire.math.UByte
import rta.model.triggers.Implicits._

@manual(Intent.ACTION_BATTERY_CHANGED -> 1.minute)
class BatteryService extends ServiceFragment[BatteryLike] {
  import Battery._

  final val handle: PF = {
    case intent if intent.getAction == Uses.actionFor[PowerState.Connected.type] =>
      PowerState.Connected
    case intent if intent.getAction == Uses.actionFor[PowerState.Disconnected.type] =>
      PowerState.Disconnected
    case intent if intent.getAction == Uses.actionFor[BatteryState.Low.type] =>
      BatteryState.Low
    case intent if intent.getAction == Uses.actionFor[BatteryState.OK.type] =>
      BatteryState.OK
    case intent if intent.getAction == Uses.actionFor[Battery] =>
      val level = UByte((intent[Int](EXTRA_LEVEL) * 100d /
        intent[Int](EXTRA_SCALE).toDouble).round.toByte)
      val present = intent[Boolean](EXTRA_PRESENT)
      val plugged = Plugged.fromInt(intent[Int](EXTRA_PLUGGED))
      val status = Status.fromInt(intent[Int](EXTRA_STATUS))

      Battery(level, plugged, present, status)
  }
}
