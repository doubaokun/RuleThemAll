package sta.services

import android.content.{Context, Intent}
import android.net.NetworkInfo
import android.net.wifi.{WifiInfo, WifiManager}
import sta.common.UsedFeatures
import sta.model.triggers.Implicits._

class WiFiService extends ServiceFragment[WiFi] {
  final val handle: PF = {
    case intent if intent.getAction == WifiManager.WIFI_STATE_CHANGED_ACTION =>
      WiFiState.intValues.get(intent.extra[Int].get(WifiManager.EXTRA_WIFI_STATE))
    case intent if intent.getAction == WifiManager.NETWORK_STATE_CHANGED_ACTION &&
      intent.extra[NetworkInfo].get(WifiManager.EXTRA_NETWORK_INFO).getState == NetworkInfo.State.CONNECTED =>
      WiFiConnection(Option(intent.extra[WifiInfo].get(WifiManager.EXTRA_WIFI_INFO)
        .getSSID.stripPrefix("\"").stripSuffix("\"")))
    case intent if intent.getAction == WifiManager.NETWORK_STATE_CHANGED_ACTION &&
      intent.extra[NetworkInfo].get(WifiManager.EXTRA_NETWORK_INFO).getState == NetworkInfo.State.DISCONNECTED =>
      WiFiConnection(None)
  }

  def reactOn: Set[String] = implicitly[UsedFeatures[WiFi]].intents
}
