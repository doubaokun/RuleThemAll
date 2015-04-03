package sta.parser

import org.parboiled2._
import sta.model.actions.Action
import sta.model.triggers.{ Trigger ⇒ Condition }
import sta.model.{ DefName, Definition ⇒ Def }
import sta.parser.actions.SystemActionRules
import sta.parser.triggers.SystemTriggerRules

import scala.collection.immutable
import scala.collection.mutable
import scala.util.control.NonFatal
import scalaz._

private class DSLParser(val input: ParserInput)
    extends SystemTriggerRules
    with SystemActionRules {
  def Name: Rule1[String @@ DefName] = rule(
    capture(oneOrMore(CharPredicate.AlphaNum)) ~> ((s: String) ⇒ Tag[String, DefName](s))
  )

  def Trigger: Rule1[Condition] = rule(
    '(' ~ MainT ~ ')'
  )

  def Action: Rule1[immutable.Seq[Action]] = rule(
    ('{' ~ oneOrMore(MainA).separatedBy(Newline1 | ';') ~ '}') ~> ((actions: immutable.Seq[Action]) ⇒ actions)
  )

  def Definition: Rule1[Def] = rule(
    ("def" ~ Name ~ '{' ~ "when" ~ Trigger ~ "do" ~ Action ~ '}') ~> Def
  )

  def Root: Rule1[Seq[Def]] = rule(
    oneOrMore(Definition) ~ EOI
  )
}

object DSLParser {
  sealed trait ParsingError extends RuntimeException { this: RuntimeException ⇒ }

  def parse(input: String): \/[ParsingError, Seq[Def]] = {
    val parser = new DSLParser(ParserInput(input))
    try {
      import org.parboiled2.Parser.DeliveryScheme.Throw
      \/-(parser.Root.run())
    } catch {
      case err: ParseError ⇒
        -\/(new RuntimeException(parser.formatError(err)) with ParsingError)
      case NonFatal(t) ⇒ -\/(new RuntimeException(t) with ParsingError)
    }
  }
}
