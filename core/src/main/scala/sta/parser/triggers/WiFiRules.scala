package sta.parser.triggers

import fastparse.noApi._
import sta.common.UsedFeatures
import sta.model.triggers.{Implicits, AtomicTrigger}
import sta.model.triggers.Implicits._

object WiFiRules extends TriggerParser[WiFi] {
  import white._

  def prefix: String = implicitly[UsedFeatures[WiFi]].category

  def state: P[AtomicTrigger[WiFiState]] = {
    mapParser(WiFiState.namesToValuesMap) map (v => AtomicTrigger[WiFiState](_ == v))
  }

  def connection: P[AtomicTrigger[WiFiConnection]] = {
    lazy val BSSID = MacAddress

    lazy val SSID = String.filter(_.length <= 32)

    P("connected" ~ "to" ~ ((BSSID map (v => AtomicTrigger[WiFiConnection] {
        case WiFiConnection.Connected(_, bssid) => v == bssid
        case _ => false
      })) | (SSID map (v => AtomicTrigger[WiFiConnection] {
        case WiFiConnection.Connected(ssid, _) => v == ssid
        case _ => false
      }))) | ("disconnected" ~ "from" ~ (
      (BSSID map (v => AtomicTrigger[WiFiConnection] {
        case WiFiConnection.Disconnected => true
        case WiFiConnection.Connected(_, bssid) => v != bssid
      })) | (SSID map (v => AtomicTrigger[WiFiConnection] {
        case WiFiConnection.Disconnected => true
        case WiFiConnection.Connected(ssid, _) => v != ssid
      }))))
    )
  }

  val Rule: P[AtomicTrigger[_ <: WiFi]] = state | connection
}
