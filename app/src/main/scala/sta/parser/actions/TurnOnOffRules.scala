package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.TurnOnOffDevice
import sta.parser.ActionParser

object TurnOnOffRules extends ActionParser[TurnOnOffDevice] {
  import white._

  val Rule: P[TurnOnOffDevice] = "turn".withWS ~! (
    "bluetooth".push(TurnOnOffDevice.Bluetooth.apply _) |
    "wifi".push(TurnOnOffDevice.WiFi.apply _)
  ) ~ ("on".push(true) | "off".push(false)) map {
    case (constructor, enable) => constructor(enable)
  }
}
