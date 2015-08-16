package sta.parser.triggers

import fastparse.noApi._
import scala.collection.mutable
import sta.model.Model
import sta.model.triggers._
import sta.parser.WhitespaceSkip

trait TriggerRules extends WhitespaceSkip {
  private val parsers = mutable.LinkedHashSet.empty[TriggerParser[_ <: Model]]
  parsers ++= Seq(BatteryRules, HeadsetRules, WiFiRules)

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
        val main = P(trigger.Rule)(trigger.prefix)
        val main2 = "(" ~ main ~ "," ~ main ~ ")"
        val main2toN = twoOrMore(main)

        val conditions = ("(" ~ (
          (main.rep(1, sep = ",") map (Trigger(_))) |
            ("or" ~ main2toN map (ts => Trigger.or(ts))) |
            ("xor" ~ main2 map (t => XorTrigger(t._1, t._2))) |
            ("and" ~ main2toN map (ts => Trigger.and(ts)))
          ) ~ ")") | main

        trigger.prefix ~ trigger.Main.fold(conditions)(ap =>
          ap ~ conditions.? map (t => t._2.fold[Trigger](t._1)(t2 => AndTrigger(t._1, t2)))
        )
      }

      P(parsers.tail.foldLeft(single(parsers.head)) {
        case (acc, t) =>
          acc | single(t)
      })
    }

    val triggers2 = "(" ~ triggers ~ "," ~ triggers ~ ")"
    val triggers2toN = twoOrMore(triggers)

    P((triggers.rep(1, sep = ",") map (Trigger(_))) |
      ("or" ~ triggers2toN map (ts => Trigger.or(ts))) |
      ("xor" ~ triggers2 map (t => XorTrigger(t._1, t._2))) |
      ("and" ~ triggers2toN map (ts => Trigger.and(ts))))
  }
}
