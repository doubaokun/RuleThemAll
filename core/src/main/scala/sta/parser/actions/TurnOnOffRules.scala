package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.TurnOnOffDevice

object TurnOnOffRules extends ActionParser[TurnOnOffDevice] {
  import white._

  val Rule: P[TurnOnOffDevice] = "turn" ~! (
    "bluetooth".l.map(_ => TurnOnOffDevice.Bluetooth.apply _) |
    "wifi".l.map(_ => TurnOnOffDevice.WiFi.apply _)
  ) ~ ("on".l.map(_=> true) | "off".l.map(_ => false)) map {
    case (constructor, enable) => constructor(enable)
  }
}
