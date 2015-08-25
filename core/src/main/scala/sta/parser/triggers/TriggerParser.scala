package sta.parser.triggers

import scala.language.implicitConversions
import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.AtomicTrigger
import sta.model.{Model, ModelCompanion}
import sta.parser.{BasicRules, WhitespaceSkip}

trait TriggerParser[M <: Model] extends BasicRules with WhitespaceSkip {
  import white._

  def matchStringParser[T <: Model : ModelCompanion: Uses](extractor: T => String): P[AtomicTrigger[T]] = {
    P(("contains" ~ String map (str => AtomicTrigger[T](extractor(_).contains(str)))) |
      ("matches" ~ String map { str =>
        val regex = str.r
        AtomicTrigger[T](m => regex.findFirstIn(extractor(m)).isDefined)
      }))
  }

  def Prefix: String
  
  def Suffix: Option[P[AtomicTrigger[_ <: M]]] = None

  def Rule: P[AtomicTrigger[_ <: M]]
}
