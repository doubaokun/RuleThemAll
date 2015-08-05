package sta.parser

import fastparse.core.SyntaxError
import fastparse.noApi._
import scalaz.\/
import sta.model.actions.Action
import sta.model.triggers.Trigger
import sta.model.{Definition => Def}
import sta.parser.actions.ActionRules
import sta.parser.triggers.TriggerRules

sealed abstract class DSLParser extends WhitespaceSkip with ActionRules with TriggerRules {
  import white._

  private def newLine = "\n" | "\n\r"

  private def alphaNum = CharIn(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z'))

  def Name: P[String] = P(alphaNum.repX(1).!)

  def Trigger: P[Trigger] = P("(" ~ MainT ~ ")")

  def Action: P[Seq[Action]] = P("{" ~ MainA.rep(1, sep = newLine | ";") ~ "}")

  def Definition: P[Def] = P(
    ("def" ~ Name ~ "{" ~ "when" ~ Trigger ~ "do" ~ Action ~ "}") map (v => Def(v._1, v._2, v._3))
  )

  def Root: P[Seq[Def]] = P(Start ~ Definition.rep(1) ~ End)
}

object DSLParser extends DSLParser {
  def parse(input: String): \/[SyntaxError, Seq[Def]] = {
    \/.fromTryCatchThrowable[Seq[Def], SyntaxError](Root.parse(input).get.value)
  }
}
