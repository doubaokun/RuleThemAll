package sta.parser.actions

import fastparse.all._
import scala.collection.mutable
import sta.model.actions.Action
import sta.parser.ActionParser

trait ActionRules {
  private[this] val parsers = mutable.HashMap.empty[Class[_], ActionParser[_ <: Action]]
  addActionParser(LaunchApplicationRules)
  addActionParser(SetToRules)
  addActionParser(TurnOnOffRules)

  def addActionParser(parser: ActionParser[_ <: Action]): Unit = {
    parsers += (parser.actionClass -> parser)
  }

  def removeActionParser(parserClass: Class[_]): Unit = {
    parsers -= parserClass
  }

  final def Action: P[Action] = {
    parsers.valuesIterator.drop(1).foldLeft(parsers.head._2.Rule.asInstanceOf[P[Action]]) {
      case (acc, p) => acc | p.Rule.asInstanceOf[P[Action]]
    }
  }
}
