package sta.parser.triggers

import fastparse.noApi._
import scala.collection.mutable
import sta.model.BaseModel
import sta.model.triggers._
import sta.parser.Extras._
import sta.parser.{Extras, TriggerParser}

trait TriggerRules {
  private[this] val parsers = mutable.HashMap.empty[String, TriggerParser[_ <: BaseModel]]
  addTriggerParser(BatteryRules)
  addTriggerParser(BluetoothRules)
  addTriggerParser(CalendarRules)
  addTriggerParser(HeadsetRules)
  addTriggerParser(NetworkRules)
  addTriggerParser(TimeRules)
  addTriggerParser(WiFiRules)

  def addTriggerParser(parser: TriggerParser[_ <: BaseModel]): Unit = {
    parsers += (parser.Prefix -> parser)
  }

  def removeTriggerParser(parserPrefix: String): Unit = {
    parsers -= parserPrefix
  }

  import white._

  final def Triggers: P[Trigger] = {
    def twoOrMore(of: P[Trigger]) = "(" ~ of.rep(2, sep = ",") ~ ")"

    val triggers = {
      def single(trigger: TriggerParser[_ <: BaseModel]) = {
        val main = trigger.Rule
        val singleErr = "single condition"
        val multipleErr = """multiple conditions inside "(...)" separated by ",""""

        val conditions = P(Skip1 ~~ main)(singleErr) | P(
          Skip0 ~~ "(" ~ (main.rep(1, sep = ",") map (ts => Trigger(ts.head, ts.tail: _*))) ~ ")"
        )(multipleErr)

        trigger.Prefix.splitWS ~~! conditions
      }

      P(parsers.valuesIterator.drop(1).foldLeft(single(parsers.head._2))(_ | single(_)))
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
