package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.{Implicits, ModelTrigger}
import sta.model.triggers.Implicits._

object WiFiRules extends TriggerParser[WiFi] {
  import white._

  def Prefix: String = Uses.categoryOf[WiFi]

  def state: P[ModelTrigger[WiFiState]] = {
    mapParser(WiFiState.namesToValuesMap) map (v => ModelTrigger[WiFiState](_ == v))
  }

  def connection: P[ModelTrigger[WiFiConnection]] = {
    lazy val SSID = String.filter(_.length <= 32)

    P("connected" ~ "to" ~ ((MacAddress map (v => ModelTrigger[WiFiConnection] {
        case WiFiConnection.Connected(_, bssid) => v == bssid
        case _ => false
      })) | (SSID map (v => ModelTrigger[WiFiConnection] {
        case WiFiConnection.Connected(ssid, _) => v == ssid
        case _ => false
      }))) | ("disconnected" ~ "from" ~ (
      (MacAddress map (v => ModelTrigger[WiFiConnection] {
        case WiFiConnection.Disconnected => true
        case WiFiConnection.Connected(_, bssid) => v != bssid
      })) | (SSID map (v => ModelTrigger[WiFiConnection] {
        case WiFiConnection.Disconnected => true
        case WiFiConnection.Connected(ssid, _) => v != ssid
      }))))
    )
  }

  val Rule: P[ModelTrigger[_ <: WiFi]] = state | connection
}
