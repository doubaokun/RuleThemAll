package rta.parser.triggers

import fastparse.noApi._
import rta.model.BaseModel
import rta.model.triggers._
import rta.parser.Extras._
import rta.parser.TriggerParser
import scala.collection.mutable

trait TriggerParsers {
  private[this] val parsers = mutable.LinkedHashMap.empty[String, TriggerParser[_ <: BaseModel]]
  addTriggerParser(BatteryParser)
  addTriggerParser(BluetoothParser)
  addTriggerParser(CalendarParser)
  addTriggerParser(DockParser)
  addTriggerParser(HeadsetParser)
  addTriggerParser(NetworkParser)
  addTriggerParser(TimeParser)
  addTriggerParser(WiFiParser)

  private[rta] def addTriggerParser(parser: TriggerParser[_ <: BaseModel]): Boolean  =
    if (parsers.contains(parser.Prefix)) false
    else {
      parsers += (parser.Prefix -> parser)
      true
    }

  private[rta] def removeTriggerParser(parserPrefix: String): Unit = parsers -= parserPrefix

  import white._

  final def Triggers: P[Trigger] = {
    def twoOrMore(of: P[Trigger]) = "(" ~ of.rep(2, sep = ",") ~ ")"

    val triggers = {
      def single(trigger: TriggerParser[_ <: BaseModel]) = {
        val main = trigger.Rule

        val conditions = (Skip1 ~~ main) |
          (Skip0 ~~ "(" ~ (main.rep(1, sep = ",") map (ts => Trigger(ts.head, ts.tail: _*))) ~ ")")

        trigger.Prefix.splitWS ~~! ((for {
          Trigger.Negate(negated) <- Skip1 ~~ "not" ~~! conditions
        } yield negated) | conditions)
      }

      parsers.valuesIterator.drop(1).foldLeft(single(parsers.head._2))(_ | single(_))
    }
    lazy val negation: P[Trigger] = for {
      Trigger.Negate(negated) <- triggers | logicOps
    } yield negated
    lazy val logicOps: P[Trigger] = P(
      ("and" ~! twoOrMore(triggers | logicOps) map (ts => Trigger.and(ts.head, ts.tail.head, ts.tail.tail: _*))) |
        ("or" ~! twoOrMore(triggers | logicOps) map (ts => Trigger.or(ts.head, ts.tail.head, ts.tail.tail: _*))) |
        ("not" ~! negation)
    )

    (triggers | logicOps).rep(1, sep = ",").map(ts => Trigger(ts.head, ts.tail: _*))
  }
}
