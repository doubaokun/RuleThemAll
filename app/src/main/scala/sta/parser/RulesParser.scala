package sta.parser

import fastparse.core.SyntaxError
import fastparse.noApi._
import sta.model.{BaseModel, Rule}
import sta.model.actions.Action
import sta.model.triggers.Trigger
import sta.parser.Extras._
import sta.parser.actions.ActionRules
import sta.parser.triggers.TriggerRules

sealed abstract class RulesParser extends Extras with ActionRules with TriggerRules {
  import white._

  def Name: P[String] = P((CharPred(_.isLetter) ~~ ("_" ~~ CharPred(_.isLetterOrDigit)).?).repX(1).! ~~ !"_")

  def Branches: P[Seq[Trigger.Branch]] = P("(" ~ Triggers.map(_.flatten) ~ ")")

  def Actions: P[Seq[Action]] = P("{" ~ Action.repX(1, sep = (WS0 ~~ NoTrace(";") ~~ WS0) | WS1) ~ "}")

  def Definition: P[Rule] = P(
    ("rule".withWS ~ Name ~ {
      "{" ~ ("when" ~! Branches).?.map(_.getOrElse(Trigger.empty.flatten)) ~ "do" ~! Actions ~ "}"
    }) map (v => Rule(v._1, v._2._1, v._2._2))
  )

  def Root: P[Seq[Rule]] = P(Start ~ Definition.rep(1) ~ End)

  def TracedRoot: P[Seq[(Rule, Int)]] = P(Start ~ (Definition ~~ Index).rep(1) ~ End)

  def Single: P[Rule] = P(Start ~ Definition ~ End)

  override final def addActionParser(parser: ActionParser[_ <: Action]): Unit = {
    super.addActionParser(parser)
  }

  override final def removeActionParser(parser: ActionParser[_ <: Action]): Unit = {
    super.removeActionParser(parser)
  }

  override final def addTriggerParser(parser: TriggerParser[_ <: BaseModel]): Unit = {
    super.addTriggerParser(parser)
  }

  override final def removeTriggerParser(parser: TriggerParser[_ <: BaseModel]): Unit = {
    super.removeTriggerParser(parser)
  }
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
