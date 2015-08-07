package sta.parser

import fastparse.core.SyntaxError
import fastparse.noApi._
import scalaz.\/
import sta.model.actions.Action
import sta.model.triggers.Trigger
import sta.model.Rule
import sta.parser.actions.ActionRules
import sta.parser.triggers.TriggerRules

sealed abstract class RulesParser extends WhitespaceSkip with ActionRules with TriggerRules {
  import white._

  private def newLine = "\n" | "\n\r"

  private def alphaNum = CharIn(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z'))

  def Name: P[String] = P(alphaNum.repX(1).!)

  def Trigger: P[Trigger] = P("(" ~ MainT ~ ")")

  def Action: P[Seq[Action]] = P("{" ~ MainA.rep(1, sep = newLine | ";") ~ "}")

  def Definition: P[Rule] = P(
    ("def" ~ Name ~ "{" ~ "when" ~ Trigger ~ "do" ~ Action ~ "}") map (v => Rule(v._1, v._2, v._3))
  )

  def Root: P[Seq[Rule]] = P(Start ~ Definition.rep(1) ~ End)
}

object RulesParser extends RulesParser {
  def parse(input: String): \/[SyntaxError, Seq[Rule]] = {
    \/.fromTryCatchThrowable[Seq[Rule], SyntaxError](Root.parse(input).get.value)
  }
}
