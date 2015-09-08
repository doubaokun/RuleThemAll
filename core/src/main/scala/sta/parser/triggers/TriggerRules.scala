package sta.parser.triggers

import fastparse.noApi._
import scala.collection.mutable
import sta.model.BaseModel
import sta.model.triggers._
import sta.parser.BasicRules._
import sta.parser.WhitespaceSkip

trait TriggerRules extends WhitespaceSkip {
  private val parsers = mutable.LinkedHashSet.empty[TriggerParser[_ <: BaseModel]]
  parsers ++= Seq(BatteryRules, BluetoothRules, CalendarRules, HeadsetRules,
    NetworkRules, TimeRules, WiFiRules)

  protected def addTriggerParser(parser: TriggerParser[_ <: BaseModel]): Unit = {
    parsers += parser
  }

  protected def removeTriggerParser(parser: TriggerParser[_ <: BaseModel]): Unit = {
    parsers -= parser
  }

  import white._

  final def MainT: P[Trigger] = {
    def twoOrMore(of: P[Trigger]) = "(" ~ of.rep(2, sep = ",") ~ ")"

    val triggers = {
      def single(trigger: TriggerParser[_ <: BaseModel]) = {
        val main = P(trigger.Rule)(trigger.Prefix)
        val main2toN = twoOrMore(main)

        val conditions = main | ("(" ~ (
          ("or" ~! main2toN map (ts => Trigger.or(ts.head, ts.tail.head, ts.tail.tail: _*))) |
            ("and" ~! main2toN map (ts => Trigger.and(ts.head, ts.tail.head, ts.tail.tail: _*))) |
            (main.rep(1, sep = ",") map (ts => Trigger(ts.head, ts.tail: _*)))
          ) ~ ")")

        trigger.Prefix.lWS ~! conditions
      }

      P(parsers.tail.foldLeft(single(parsers.head))(_ | single(_)))
    }
    lazy val combined2ToN = P(twoOrMore(triggers | all))
    lazy val combined: P[Trigger] = P(triggers | all)
    lazy val all: P[Trigger] = P(
      ("or" ~! combined2ToN map (ts => Trigger.or(ts.head, ts.tail.head, ts.tail.tail: _*))) |
        ("and" ~! combined2ToN map (ts => Trigger.and(ts.head, ts.tail.head, ts.tail.tail: _*))) |
        (combined.rep(1, sep = ",") map (ts => Trigger(ts.head, ts.tail: _*)))
    )

    all
  }
}
