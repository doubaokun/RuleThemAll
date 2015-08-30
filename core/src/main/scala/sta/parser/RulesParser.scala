package sta.parser

import fastparse.core.SyntaxError
import fastparse.noApi._
import sta.model.Rule
import sta.model.actions.Action
import sta.model.triggers.Condition
import sta.parser.actions.ActionRules
import sta.parser.triggers.TriggerRules

sealed abstract class RulesParser extends WhitespaceSkip with ActionRules with TriggerRules {
  import white._

  private def newLine = "\n" | "\n\r"

  private def alphaNum = CharIn(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z'))

  def Name: P[String] = P((alphaNum ~ ("_" ~ alphaNum).?).repX(1).!)

  def Branches: P[Seq[Condition.Branch]] = P("(" ~ MainT.map(_.flatten) ~ ")")

  def Action: P[Seq[Action]] = P("{" ~ MainA.rep(1, sep = newLine | ";") ~ "}")

  def Definition: P[Rule] = P(
    ("def" ~ Name ~ {
      ("{" ~ "when" ~! Branches ~ "do" ~! Action ~ "}") |
        ((Pass map (_ => Condition.empty.flatten)) ~ Action)
    }) map (v => Rule(v._1, v._2._1, v._2._2))
  )

  def Root: P[Seq[Rule]] = P(Start ~ Definition.rep(1) ~ End)
  
  def TracedRoot: P[Seq[(Rule, Int)]] = P(Start ~ (Definition ~ Index).rep(1) ~ End)
  
  def Single: P[Rule] = P(Start ~ Definition ~ End)
}

object RulesParser extends RulesParser {
  def parse(input: String): Either[SyntaxError, Seq[Rule]] = {
    try {
      Right(Root.parse(input).get.value)
    } catch {
      case se: SyntaxError => Left(se)
    }
  }
  
  def tracedParse(input: String): Either[SyntaxError, Seq[(Rule, Int)]] = {
    try {
      Right(TracedRoot.parse(input).get.value)
    } catch {
      case se: SyntaxError => Left(se)
    }
  }

  protected[sta] def parseSingle(input: String): Rule = Single.parse(input).get.value
}
