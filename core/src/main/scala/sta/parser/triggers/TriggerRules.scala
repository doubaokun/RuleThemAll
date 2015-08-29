package sta.parser.triggers

import fastparse.noApi._
import scala.collection.mutable
import sta.model.Model
import sta.model.triggers._
import sta.parser.WhitespaceSkip

trait TriggerRules extends WhitespaceSkip {
  private val parsers = mutable.LinkedHashSet.empty[TriggerParser[_ <: Model]]
  parsers ++= Seq(BatteryRules, BluetoothRules, CalendarRules, DateRules, HeadsetRules, WiFiRules)

  protected def addTriggerParser(parser: TriggerParser[_ <: Model]): Unit = {
    parsers += parser
  }

  protected def removeTriggerParser(parser: TriggerParser[_ <: Model]): Unit = {
    parsers -= parser
  }

  import white._

  final def MainT: P[Trigger] = {
    def twoOrMore(of: P[Trigger]) = "(" ~ of.rep(2, sep = ",") ~ ")"

    val triggers = {
      def single(trigger: TriggerParser[_ <: Model]) = {
        val prefix = {
          val splitted = trigger.Prefix.split("\\s+")
          splitted.tail.foldLeft(splitted.head: P[Unit]) { _ ~ _ }
        }
        val main = P(trigger.Rule)(trigger.Prefix)
        val main2 = "(" ~ main ~ "," ~ main ~ ")"
        val main2toN = twoOrMore(main)

        val conditions = ("(" ~ (
          (main.rep(1, sep = ",") map (ts => Trigger(ts.head, ts.tail: _*))) |
            ("or" ~ main2toN map (ts => Trigger.or(ts.head, ts.tail.head, ts.tail.tail: _*))) |
            ("xor" ~ main2 map (t => Trigger.xor(t._1, t._2))) |
            ("and" ~ main2toN map (ts => Trigger.and(ts.head, ts.tail.head, ts.tail.tail: _*)))
          ) ~ ")") | main

        prefix ~! trigger.Suffix.fold(conditions)(suffix =>
          conditions.? ~! suffix map (t => t._1.fold[Trigger](t._2)(r => Trigger.and(t._2, r)))
        )
      }

      P(parsers.tail.foldLeft(single(parsers.head)) {
        case (acc, t) =>
          acc | single(t)
      })
    }

    val triggers2 = "(" ~ triggers ~ "," ~ triggers ~ ")"
    val triggers2toN = twoOrMore(triggers)

    P((triggers.rep(1, sep = ",") map (ts => Trigger(ts.head, ts.tail: _*))) |
      ("or" ~ triggers2toN map (ts => Trigger.or(ts.head, ts.tail.head, ts.tail.tail: _*))) |
      ("xor" ~ triggers2 map (t => Trigger.xor(t._1, t._2))) |
      ("and" ~ triggers2toN map (ts => Trigger.and(ts.head, ts.tail.head, ts.tail.tail: _*))))
  }
}
