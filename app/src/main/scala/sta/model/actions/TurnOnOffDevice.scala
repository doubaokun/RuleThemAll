package sta.model.actions

import android.bluetooth.BluetoothAdapter
import android.content.Context
import kj.android.common.SystemServices._

sealed abstract class TurnOnOffDevice extends Action { this: Product =>
  def enable: Boolean
}

object TurnOnOffDevice {
  case class Bluetooth(enable: Boolean) extends TurnOnOffDevice {
    def execute()(implicit ctx: Context): Unit = enable match {
      case true => BluetoothAdapter.getDefaultAdapter.enable()
      case false => BluetoothAdapter.getDefaultAdapter.disable()
    }
  }

  case class WiFi(enable: Boolean) extends TurnOnOffDevice {
    def execute()(implicit ctx: Context): Unit = wifiManager.setWifiEnabled(enable)
  }
}
