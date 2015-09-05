package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.TurnOnOffDevice

object TurnOnOffRules extends ActionParser[TurnOnOffDevice] {
  import white._

  val Rule: P[TurnOnOffDevice] = "turn" ~ (
    "bluetooth".u.map(_ => TurnOnOffDevice.Bluetooth.apply _) |
    "wifi".u.map(_ => TurnOnOffDevice.WiFi.apply _)
  ) ~ ("on".u.map(_=> true) | "off".u.map(_ => false)) map {
    case (constructor, enable) => constructor(enable)
  }
}
