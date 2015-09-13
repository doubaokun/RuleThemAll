package sta.model.triggers

import android.net.{ConnectivityManager, NetworkInfo}
import enumeratum.Enum
import sta.common.{action, category}
import sta.model._

trait NetworkModels {
  @category("network")
  @action(ConnectivityManager.CONNECTIVITY_ACTION)
  case class Network(connection: Network.Connection, state: Network.State) extends MultiModel[Network](Network) {
    def sameAs(other: Network): Boolean = connection == other.connection
  }

  implicit object Network extends MultiModelCompanion[Network] {
    sealed abstract class Connection extends FromIntEntry // TODO rest of connection types
    object Connection extends Enum[Connection] with FromInt[Connection] {
      lazy val values = findValues

      case object Bluetooth extends Connection {
        def intValue: Int = ConnectivityManager.TYPE_BLUETOOTH
      }
      case object Ethernet extends Connection {
        def intValue: Int = ConnectivityManager.TYPE_ETHERNET
      }
      case object Mobile extends Connection {
        def intValue: Int = ConnectivityManager.TYPE_MOBILE
      }
      case object Wifi extends Connection {
        def intValue: Int = ConnectivityManager.TYPE_WIFI
      }
    }
    
    sealed abstract class State extends FromJavaEnumEntry[NetworkInfo.State]
    object State extends Enum[State] with FromJavaEnum[NetworkInfo.State, State] {
      lazy val values = findValues

      case object Connected extends State {
        def enumValue: NetworkInfo.State = NetworkInfo.State.CONNECTED
      }
      case object Disconnected extends State {
        def enumValue: NetworkInfo.State = NetworkInfo.State.DISCONNECTED
      }
    }
  }
}
