package rta.parser.triggers

import fastparse.noApi._
import java.nio.charset.Charset
import rta.common.Uses
import rta.model.triggers.Implicits._
import rta.model.triggers.Trigger
import rta.parser.TriggerParser

object BluetoothRules extends TriggerParser[Bluetooth] {
  import white._

  def Prefix: String = Uses.categoryOf[Bluetooth]

  def state: P[Trigger.Condition[BluetoothState]] = {
    mapParser(BluetoothState.namesToValuesMap).map(v => Trigger.Condition[BluetoothState](_ == v))
  }

  def connection: P[Trigger.Condition[BluetoothConnection]] = {
    lazy val Name = String.filter(_.getBytes(Charset.forName("UTF-8")).length <= 248)

    P("connected".withWS ~ "to".withWS ~ (MacAddress.map(v => Trigger.Condition[BluetoothConnection] {
        case BluetoothConnection.Connected(_, address) => v == address
        case _ => false
      }) | (Name map (v => Trigger.Condition[BluetoothConnection] {
        case BluetoothConnection.Connected(name, _) => v == name
        case _ => false
      }))) | ("disconnected".withWS ~ "from".withWS ~ (
      MacAddress.map(v => Trigger.Condition[BluetoothConnection] {
        case BluetoothConnection.Disconnected => true
        case BluetoothConnection.Connected(_, address) => v != address
      }) | Name.map(v => Trigger.Condition[BluetoothConnection] {
        case BluetoothConnection.Disconnected => true
        case BluetoothConnection.Connected(name, _) => v != name
      })))
    )
  }

  def Main: P[Trigger.Standalone[_ <: Bluetooth]] = state | connection
}
