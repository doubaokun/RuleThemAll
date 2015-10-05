package rta.service

import android.net.NetworkInfo
import android.net.wifi.{WifiInfo, WifiManager}
import rta.model.triggers.Implicits._

class WiFiService extends ServiceFragment[WiFi] {
  final val handle: PF = {
    case intent if intent.getAction == WifiManager.WIFI_STATE_CHANGED_ACTION =>
      WiFiState.intValues.get(intent[Int](WifiManager.EXTRA_WIFI_STATE))
    case intent if intent.getAction == WifiManager.NETWORK_STATE_CHANGED_ACTION &&
      intent[NetworkInfo](WifiManager.EXTRA_NETWORK_INFO).getState == NetworkInfo.State.CONNECTED =>
      WiFiConnection.Connected(
        ssid = intent[WifiInfo](WifiManager.EXTRA_WIFI_INFO).getSSID.stripPrefix("\"").stripSuffix("\""),
        bssid = intent[String](WifiManager.EXTRA_BSSID)
      )
    case intent if intent.getAction == WifiManager.NETWORK_STATE_CHANGED_ACTION &&
      intent[NetworkInfo](WifiManager.EXTRA_NETWORK_INFO).getState == NetworkInfo.State.DISCONNECTED =>
      WiFiConnection.Disconnected
  }
}
