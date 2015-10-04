package sta.service

import android.bluetooth.{BluetoothAdapter, BluetoothDevice}
import sta.model.triggers.Implicits._

class BluetoothService extends ServiceFragment[Bluetooth] {
  final val handle: PF = {
    case intent if intent.getAction == BluetoothAdapter.ACTION_STATE_CHANGED =>
      BluetoothState.intValues.get(intent[Int](BluetoothAdapter.EXTRA_STATE))
    case intent if intent.getAction == BluetoothAdapter.ACTION_CONNECTION_STATE_CHANGED &&
      intent[Int](BluetoothAdapter.EXTRA_CONNECTION_STATE) == BluetoothAdapter.STATE_CONNECTED =>
      val device = intent[BluetoothDevice](BluetoothDevice.EXTRA_DEVICE)
      BluetoothConnection.Connected(
        name = device.getName,
        address = device.getAddress
      )
    case intent if intent.getAction == BluetoothAdapter.ACTION_CONNECTION_STATE_CHANGED &&
      intent[Int](BluetoothAdapter.EXTRA_CONNECTION_STATE) == BluetoothAdapter.STATE_DISCONNECTED =>
      BluetoothConnection.Disconnected
  }
}