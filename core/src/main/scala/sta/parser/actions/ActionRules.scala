package sta.parser.actions

import fastparse.all._
import scala.collection.mutable
import sta.model.actions.Action

trait ActionRules {
  private val parsers = mutable.LinkedHashSet.empty[ActionParser]
  parsers ++= Seq(SoundProfileRules)

  protected def addActionParser(parser: ActionParser): Unit = {
    parsers += parser
  }

  protected def removeActionParser(parser: ActionParser): Unit = {
    parsers -= parser
  }

  final def MainA: P[Action] = {
    parsers.tail.foldLeft(parsers.head.Main) { case (acc, p) => acc | p.Main }
  }
}
