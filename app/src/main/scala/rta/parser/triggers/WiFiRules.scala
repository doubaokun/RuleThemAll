package rta.parser.triggers

import fastparse.noApi._
import rta.common.Uses
import rta.model.triggers.Implicits._
import rta.model.triggers.Trigger
import rta.parser.TriggerParser

object WiFiRules extends TriggerParser[WiFi] {
  import white._

  def Prefix: String = Uses.categoryOf[WiFi]

  def state: P[Trigger.Condition[WiFiState]] = {
    mapParser(WiFiState.namesToValuesMap) map (v => Trigger.Condition[WiFiState](_ == v))
  }

  def connection: P[Trigger.Condition[WiFiConnection]] = {
    lazy val SSID = String.filter(_.length <= 32)

    P("connected".withWS ~ "to".withWS ~ ((MacAddress map (v => Trigger.Condition[WiFiConnection] {
        case WiFiConnection.Connected(_, bssid) => v == bssid
        case _ => false
      })) | (SSID map (v => Trigger.Condition[WiFiConnection] {
        case WiFiConnection.Connected(ssid, _) => v == ssid
        case _ => false
      }))) | ("disconnected".withWS ~ "from".withWS ~ (
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
