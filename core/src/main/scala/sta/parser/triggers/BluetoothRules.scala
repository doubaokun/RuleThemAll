package sta.parser.triggers

import fastparse.noApi._
import java.nio.charset.Charset
import sta.common.Uses
import sta.model.triggers.ModelTrigger
import sta.model.triggers.Implicits._

object BluetoothRules extends TriggerParser[Bluetooth] {
  import white._

  def Prefix: String = Uses.categoryOf[Bluetooth]

  def state: P[ModelTrigger[BluetoothState]] = {
    mapParser(BluetoothState.namesToValuesMap) map (v => ModelTrigger[BluetoothState](_ == v))
  }

  def connection: P[ModelTrigger[BluetoothConnection]] = {
    lazy val Name = String.filter(_.getBytes(Charset.forName("UTF-8")).length <= 248)

    P("connected" ~ "to" ~ ((MacAddress map (v => ModelTrigger[BluetoothConnection] {
        case BluetoothConnection.Connected(_, address) => v == address
        case _ => false
      })) | (Name map (v => ModelTrigger[BluetoothConnection] {
        case BluetoothConnection.Connected(name, _) => v == name
        case _ => false
      }))) | ("disconnected" ~ "from" ~ (
      (MacAddress map (v => ModelTrigger[BluetoothConnection] {
        case BluetoothConnection.Disconnected => true
        case BluetoothConnection.Connected(_, address) => v != address
      })) | (Name map (v => ModelTrigger[BluetoothConnection] {
        case BluetoothConnection.Disconnected => true
        case BluetoothConnection.Connected(name, _) => v != name
      }))))
    )
  }

  val Rule: P[ModelTrigger[_ <: Bluetooth]] = state | connection
}
