package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.{Implicits, Trigger}
import sta.model.triggers.Implicits._

object WiFiRules extends TriggerParser[WiFi] {
  import white._

  def Prefix: String = Uses.categoryOf[WiFi]

  def state: P[Trigger.Atomic[WiFiState]] = {
    mapParser(WiFiState.namesToValuesMap) map (v => Trigger.Atomic[WiFiState](_ == v))
  }

  def connection: P[Trigger.Atomic[WiFiConnection]] = {
    lazy val SSID = String.filter(_.length <= 32)

    P("connected" ~ "to" ~ ((MacAddress map (v => Trigger.Atomic[WiFiConnection] {
        case WiFiConnection.Connected(_, bssid) => v == bssid
        case _ => false
      })) | (SSID map (v => Trigger.Atomic[WiFiConnection] {
        case WiFiConnection.Connected(ssid, _) => v == ssid
        case _ => false
      }))) | ("disconnected" ~ "from" ~ (
      (MacAddress map (v => Trigger.Atomic[WiFiConnection] {
        case WiFiConnection.Disconnected => true
        case WiFiConnection.Connected(_, bssid) => v != bssid
      })) | (SSID map (v => Trigger.Atomic[WiFiConnection] {
        case WiFiConnection.Disconnected => true
        case WiFiConnection.Connected(ssid, _) => v != ssid
      }))))
    )
  }

  val Rule: P[Trigger.Atomic[_ <: WiFi]] = state | connection
}
