package rta.service

import android.bluetooth.{BluetoothAdapter, BluetoothDevice}
import rta.common.Uses
import rta.model.triggers.Implicits._

class BluetoothService extends ServiceFragment[Bluetooth] {
  final val handle: PF = {
    case intent if intent.getAction == Uses.actionFor[BluetoothState] =>
      BluetoothState.intValues.get(intent[Int](BluetoothAdapter.EXTRA_STATE))
    case intent if intent.getAction == Uses.actionFor[BluetoothConnection] &&
      intent[Int](BluetoothAdapter.EXTRA_CONNECTION_STATE) == BluetoothAdapter.STATE_CONNECTED =>
      val device = intent[BluetoothDevice](BluetoothDevice.EXTRA_DEVICE)
      BluetoothConnection.Connected(
        name = device.getName,
        address = device.getAddress
      )
    case intent if intent.getAction == Uses.actionFor[BluetoothConnection] &&
      intent[Int](BluetoothAdapter.EXTRA_CONNECTION_STATE) == BluetoothAdapter.STATE_DISCONNECTED =>
      BluetoothConnection.Disconnected
  }
}
