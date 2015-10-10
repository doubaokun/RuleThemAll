package rta.parser.actions

import fastparse.all._
import scala.collection.mutable
import rta.model.actions.Action
import rta.parser.ActionParser

trait ActionParsers {
  private[this] val parsers = mutable.LinkedHashMap.empty[Class[_], ActionParser[_ <: Action]]
  addActionParser(AlterApplicationParser)
  addActionParser(SetToParser)
  addActionParser(TurnOnOffParser)

  private[rta] def addActionParser(parser: ActionParser[_ <: Action]): Unit = {
    parsers += (parser.actionClass -> parser)
  }

  private[rta] def removeActionParser(parserClass: Class[_]): Unit = {
    parsers -= parserClass
  }

  final def Action: P[Action] = {
    parsers.valuesIterator.drop(1).foldLeft(parsers.head._2.Rule.asInstanceOf[P[Action]]) {
      case (acc, p) => acc | p.Rule.asInstanceOf[P[Action]]
    }
  }
}
