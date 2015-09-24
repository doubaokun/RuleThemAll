package sta.parser

import fastparse.core.Result
import fastparse.noApi._
import sta.model.Rule
import sta.model.actions.Action
import sta.model.triggers.Trigger
import sta.parser.Extras._
import sta.parser.actions.ActionRules
import sta.parser.triggers.TriggerRules

object RulesParser extends Extras with ActionRules with TriggerRules {

  import white._

  def Name: P[String] = P((CharPred(_.isLetter) ~~ ("_" ~~ CharPred(_.isLetterOrDigit)).?).repX(1).! ~~ !"_")

  def Branches: P[Seq[Trigger.Branch]] = P("(" ~ Triggers.map(_.flatten) ~ ")")

  def Actions: P[Seq[Action]] = P(
    "{" ~ Action.repX(1, sep = Comment.? ~~ ((WS0 ~~ NoTrace(";") ~~ WS0) | WS1) ~~ Skip0) ~ "}"
  )

  def Definition: P[Rule] = P(
    ("rule".withWS ~ Name ~ {
      "{" ~ ("when" ~! Branches).?.map(_.getOrElse(Trigger.empty.flatten)) ~ "do" ~! Actions ~ "}"
    }) map (v => Rule(v._1, v._2._1, v._2._2))
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
