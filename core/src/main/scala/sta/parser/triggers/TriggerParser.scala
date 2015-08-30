package sta.parser.triggers

import scala.language.implicitConversions
import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Condition
import sta.model.{Model, ModelCompanion}
import sta.parser.{BasicRules, WhitespaceSkip}

trait TriggerParser[M <: Model] extends BasicRules with WhitespaceSkip {
  import white._

  def matchStringParser[T <: Model : ModelCompanion: Uses](extractor: T => String): P[Condition.Trigger[T]] = {
    P(("contains" ~ String map (str => Condition.Trigger[T](extractor(_).contains(str)))) |
      ("matches" ~ String map { str =>
        val regex = str.r
        Condition.Trigger[T](m => regex.findFirstIn(extractor(m)).isDefined)
      }))
  }

  def Prefix: String
  
  def Suffix: Option[P[Condition.Trigger[_ <: M]]] = None

  def Rule: P[Condition.Standalone[_ <: M]]
}
