package sta.model.actions

import android.bluetooth.BluetoothAdapter
import android.content.Context
import sta.common.SystemServices._

sealed abstract class TurnOnOffDevice extends Action { this: Product =>
  def enable: Boolean
}

object TurnOnOffDevice {
  final case class Bluetooth(enable: Boolean) extends TurnOnOffDevice {
    def execute()(implicit ctx: Context): Unit = enable match {
      case true => BluetoothAdapter.getDefaultAdapter.enable()
      case false => BluetoothAdapter.getDefaultAdapter.disable()
    }
  }

  final case class WiFi(enable: Boolean) extends TurnOnOffDevice {
    def execute()(implicit ctx: Context): Unit = wifiManager.setWifiEnabled(enable)
  }
}
