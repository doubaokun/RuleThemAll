package rta.model.triggers

import android.net.wifi.WifiManager
import enumeratum.Enum
import rta.common.{action, category}
import rta.model._

trait WiFiModels {
  @category("wifi")
  sealed abstract class WiFi(companion: ModelCompanion[WiFi]) extends Model[WiFi](companion)

  @action(WifiManager.WIFI_STATE_CHANGED_ACTION)
  sealed abstract class WiFiState extends WiFi(WiFiState) with FromIntEntry
  implicit object WiFiState extends ModelCompanion[WiFiState] with Enum[WiFiState] with FromInt[WiFiState] {
    lazy val values = findValues

    case object On extends WiFiState {
      def intValue: Int = WifiManager.WIFI_STATE_ENABLED
    }
    case object Off extends WiFiState {
      def intValue: Int = WifiManager.WIFI_STATE_DISABLED
    }
  }

  @action(WifiManager.NETWORK_STATE_CHANGED_ACTION)
  sealed abstract class WiFiConnection extends WiFi(WiFiConnection)
  implicit object WiFiConnection extends ModelCompanion[WiFiConnection] {
    case object Disconnected extends WiFiConnection
    case class Connected(ssid: String, bssid: String) extends WiFiConnection
  }
}
