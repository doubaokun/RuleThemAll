package sta.services

import android.net.NetworkInfo
import android.net.wifi.{WifiInfo, WifiManager}
import sta.model.triggers.Implicits._

class WiFiService extends ServiceFragment[WiFi] {
  final val handle: PF = {
    case intent if intent.getAction == WifiManager.WIFI_STATE_CHANGED_ACTION =>
      WiFiState.intValues.get(intent.extra[Int](WifiManager.EXTRA_WIFI_STATE))
    case intent if intent.getAction == WifiManager.NETWORK_STATE_CHANGED_ACTION &&
      intent.extra[NetworkInfo](WifiManager.EXTRA_NETWORK_INFO).getState == NetworkInfo.State.CONNECTED =>
      WiFiConnection.Connected(
        ssid = intent.extra[WifiInfo](WifiManager.EXTRA_WIFI_INFO).getSSID.stripPrefix("\"").stripSuffix("\""),
        bssid = intent.extra[String](WifiManager.EXTRA_BSSID)
      )
    case intent if intent.getAction == WifiManager.NETWORK_STATE_CHANGED_ACTION &&
      intent.extra[NetworkInfo](WifiManager.EXTRA_NETWORK_INFO).getState == NetworkInfo.State.DISCONNECTED =>
      WiFiConnection.Disconnected
  }
}
