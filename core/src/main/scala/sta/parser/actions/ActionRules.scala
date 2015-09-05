package sta.parser.actions

import fastparse.all._
import scala.collection.mutable
import sta.model.actions.Action

trait ActionRules {
  private val parsers = mutable.LinkedHashSet.empty[ActionParser[_ <: Action]]
  parsers ++= Seq(SetToRules, TurnOnOffRules)

  protected def addActionParser(parser: ActionParser[_ <: Action]): Unit = {
    parsers += parser
  }

  protected def removeActionParser(parser: ActionParser[_ <: Action]): Unit = {
    parsers -= parser
  }

  final def MainA: P[Action] = {
    parsers.tail.foldLeft(parsers.head.Rule.asInstanceOf[P[Action]]) {
      case (acc, p) => acc | p.Rule.asInstanceOf[P[Action]]
    }
  }
}
