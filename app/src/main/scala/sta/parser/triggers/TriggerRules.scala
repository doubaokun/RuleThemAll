package sta.parser.triggers

import fastparse.noApi._
import scala.collection.mutable
import sta.model.BaseModel
import sta.model.triggers._
import sta.parser.{TriggerParser, Extras}

trait TriggerRules extends Extras {
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

  final def Triggers: P[Trigger] = {
    def twoOrMore(of: P[Trigger]) = "(" ~ of.rep(2, sep = ",") ~ ")"

    val triggers = {
      def single(trigger: TriggerParser[_ <: BaseModel]) = {
        val main = P(trigger.Rule)(trigger.Prefix)

        val conditions = main |
          ("(" ~ (main.rep(1, sep = ",") map (ts => Trigger(ts.head, ts.tail: _*))) ~ ")")

        trigger.Prefix.splitWS.withWS ~! conditions
      }

      P(parsers.tail.foldLeft(single(parsers.head))(_ | single(_)))
    }
    lazy val logicOps: P[Trigger] = P(
      ("and" ~! twoOrMore(nested) map (ts => Trigger.and(ts.head, ts.tail.head, ts.tail.tail: _*))) |
        ("or" ~! twoOrMore(nested) map (ts => Trigger.or(ts.head, ts.tail.head, ts.tail.tail: _*)))
    )
    lazy val nested: P[Trigger] = P(triggers | logicOps)
    lazy val all: P[Trigger] = P(
      nested.rep(1, sep = ",").map(ts => Trigger(ts.head, ts.tail: _*)) | logicOps
    )

    all
  }
}
