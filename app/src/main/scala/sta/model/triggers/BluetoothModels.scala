package sta.model.triggers

import android.bluetooth.{BluetoothAdapter, BluetoothManager}
import enumeratum.Enum
import sta.common.{action, category}
import sta.model.{FromIntEntry, FromInt, ModelCompanion, Model}

trait BluetoothModels {
  @category("bluetooth")
  sealed abstract class Bluetooth(companion: ModelCompanion[Bluetooth]) extends Model[Bluetooth](companion)

  @action(BluetoothAdapter.ACTION_STATE_CHANGED)
  sealed abstract class BluetoothState extends Bluetooth(BluetoothState) with FromIntEntry
  implicit object BluetoothState extends ModelCompanion[BluetoothState]
    with Enum[BluetoothState] with FromInt[BluetoothState] {
    lazy val values = findValues

    case object On extends BluetoothState {
      def intValue: Int = BluetoothAdapter.STATE_ON
    }
    case object Off extends BluetoothState {
      def intValue: Int = BluetoothAdapter.STATE_OFF
    }
  }

  @action(BluetoothAdapter.ACTION_CONNECTION_STATE_CHANGED)
  sealed abstract class BluetoothConnection extends Bluetooth(BluetoothConnection)
  implicit object BluetoothConnection extends ModelCompanion[BluetoothConnection] {
    case object Disconnected extends BluetoothConnection
    case class Connected(name: String, address: String) extends BluetoothConnection
  }
}
