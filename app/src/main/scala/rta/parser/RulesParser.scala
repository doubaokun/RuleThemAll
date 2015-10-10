package rta.parser

import fastparse.core.Result
import fastparse.noApi._
import rta.model.Rule
import rta.model.actions.Action
import rta.model.triggers.Trigger
import rta.parser.BasicRules._
import rta.parser.Extras._
import rta.parser.actions.ActionParsers
import rta.parser.triggers.ConditionParsers
import spire.syntax.literals._

object RulesParser extends ActionParsers with ConditionParsers {

  import white._

  def Name: P[String] = P(
    (CharPred(c => c.isLetter | c == '_') ~~
      CharsWhile(c => c.isLetterOrDigit | c == '_', min = 0)).!
  )

  def Branches: P[Seq[Trigger.Branch]] = P("(" ~ Triggers.map(_.flatten) ~ ")")

  def Actions: P[Seq[Action]] = P(
    "{" ~ Action.repX(1, sep = Comment.? ~~ ((WS0 ~~ NoTrace(";") ~~ WS0) | WS1) ~~ Skip0) ~ "}"
  )

  def Definition: P[Rule] = P(
    ("rule".withWS ~ Name ~ ("with".withWS ~ "priority".withWS ~ Byte).? ~ {
      "{" ~ ("when" ~! Branches).?.map(_.getOrElse(Trigger.empty.flatten)) ~ "do" ~! Actions ~ "}"
    }) map (v => Rule(v._1, v._2.getOrElse(ub"127"), v._3._1, v._3._2))
  )

  def Multi: P[Seq[Rule]] = P(Start ~ Definition.rep(1) ~ End)

  /** Parses multiple rules from input annotated with index where the rule ends. */
  def AnnotatedMulti: P[Seq[(Rule, Int)]] = P(Start ~ (Definition ~~ Index).rep(1) ~ End)

  def Single: P[Rule] = P(Start ~ Definition ~ End)

  /** Returns initialized version of selected parser. */
  def cached[T](selector: this.type => P[T]): String => Result[T] = {
    val parser = selector(this)
    str => parser.parse(str)
  }
}
