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
    lazy val ssid = String.filter(_.length <= 32)

    ("connected" ~ "to" ~ ssid map (v => AtomicTrigger[WiFiConnection](_.ssid.exists(_ == v)))) |
      ("disconnected" ~ "from" ~ ssid map (v => AtomicTrigger[WiFiConnection](_.ssid.forall(_ != v))))
  }

  val Rule: P[AtomicTrigger[_ <: WiFi]] = state | connection
}
