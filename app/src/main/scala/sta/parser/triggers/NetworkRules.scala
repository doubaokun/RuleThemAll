package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Implicits._
import sta.model.triggers.Trigger
import sta.parser.TriggerParser

object NetworkRules extends TriggerParser[Network] {
  import Network._
  import white._

  def Prefix: String = Uses.categoryOf[Network]

  private def network: P[Trigger.Condition[Network]] =
    (mapParser(Connection.namesToValuesMap) ~ mapParser(State.namesToValuesMap)).map {
      case (connection, state) =>
        val network = Network(connection, state)
        Trigger.Condition[Network](_ == network)
    } | mapParser(State.namesToValuesMap)map(state => Trigger.Condition[Network](_.state == state))

  def Main: P[Trigger.Standalone[_ <: Network]] = network
}
