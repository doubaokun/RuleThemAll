package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Trigger
import sta.model.triggers.Implicits._
import sta.parser.TriggerParser

object WiFiRules extends TriggerParser[WiFi] {
  import white._

  def Prefix: String = Uses.categoryOf[WiFi]

  def state: P[Trigger.Condition[WiFiState]] = {
    mapParser(WiFiState.namesToValuesMap) map (v => Trigger.Condition[WiFiState](_ == v))
  }

  def connection: P[Trigger.Condition[WiFiConnection]] = {
    lazy val SSID = String.filter(_.length <= 32)

    P("connected" ~ "to" ~ ((MacAddress map (v => Trigger.Condition[WiFiConnection] {
        case WiFiConnection.Connected(_, bssid) => v == bssid
        case _ => false
      })) | (SSID map (v => Trigger.Condition[WiFiConnection] {
        case WiFiConnection.Connected(ssid, _) => v == ssid
        case _ => false
      }))) | ("disconnected" ~ "from" ~ (
      (MacAddress map (v => Trigger.Condition[WiFiConnection] {
        case WiFiConnection.Disconnected => true
        case WiFiConnection.Connected(_, bssid) => v != bssid
      })) | (SSID map (v => Trigger.Condition[WiFiConnection] {
        case WiFiConnection.Disconnected => true
        case WiFiConnection.Connected(ssid, _) => v != ssid
      }))))
    )
  }

  def Main: P[Trigger.Standalone[_ <: WiFi]] = state | connection
}
