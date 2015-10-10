package rta.parser.actions

import fastparse.noApi._
import rta.model.actions.TurnOnOff
import rta.parser.ActionParser

object TurnOnOffParser extends ActionParser[TurnOnOff] {
  import white._

  def Rule: P[TurnOnOff] = "turn".withWS ~ (
    "airplane" ~ "mode".push(TurnOnOff.AirplaneMode.apply _) |
      "bluetooth".push(TurnOnOff.Bluetooth.apply _) |
      "mobile" ~ "network".push(TurnOnOff.MobileNetwork.apply _) |
      "nfc".push(TurnOnOff.NFC.apply _) |
      "wifi".push(TurnOnOff.WiFi.apply _)
  ) ~ ("on".push(true) | "off".push(false)) map {
    case (constructor, enable) => constructor(enable)
  }
}
