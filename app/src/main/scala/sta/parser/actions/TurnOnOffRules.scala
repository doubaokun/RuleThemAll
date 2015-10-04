package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.TurnOnOff
import sta.parser.ActionParser

object TurnOnOffRules extends ActionParser[TurnOnOff] {
  import white._

  val Rule: P[TurnOnOff] = "turn".withWS ~! (
    "airplane" ~ "mode".push(TurnOnOff.AirplaneMode.apply _) |
      "bluetooth".push(TurnOnOff.Bluetooth.apply _) |
      "mobile" ~ "network".push(TurnOnOff.MobileNetwork.apply _) |
      "wifi".push(TurnOnOff.WiFi.apply _)
  ) ~ ("on".push(true) | "off".push(false)) map {
    case (constructor, enable) => constructor(enable)
  }
}
