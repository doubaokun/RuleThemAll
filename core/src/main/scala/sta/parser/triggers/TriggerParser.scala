package sta.parser.triggers

import scala.language.implicitConversions
import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Trigger
import sta.model.{BaseModel, BaseModelCompanion}
import sta.parser.{BasicRules, WhitespaceSkip}

abstract class TriggerParser[M <: BaseModel] extends BasicRules with WhitespaceSkip {
  import white._

  def matchStringParser[T <: BaseModel: BaseModelCompanion: Uses](extractor: T => String): P[Trigger.Condition[T]] = {
    P(("contains" ~ String map (str => Trigger.Condition[T](extractor(_).contains(str)))) |
      ("matches" ~ String map { str =>
        val regex = str.r
        Trigger.Condition[T](m => regex.findFirstIn(extractor(m)).isDefined)
      }))
  }

  def Prefix: String

  def Main: P[Trigger.Standalone[_ <: M]]

  lazy val Rule: P[Trigger] = Main
}
