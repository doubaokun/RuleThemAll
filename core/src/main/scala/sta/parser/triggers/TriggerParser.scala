package sta.parser.triggers

import scala.language.implicitConversions

import fastparse.all._
import kj.android.common.UsedFeatures
import sta.model.triggers.AtomicTrigger
import sta.model.{Model, ModelCompanion}
import sta.parser.BasicRules

trait TriggerParser[M <: Model] extends BasicRules {
  def matchStringParser[T <: Model : ModelCompanion: UsedFeatures](extractor: T => String): P[AtomicTrigger[T]] = {
    ("contains" ~ String map (str => AtomicTrigger[T](extractor(_).contains(str)))) |
      ("matches" ~ String map { str =>
        val regex = str.r
        AtomicTrigger[T](m => regex.findFirstIn(extractor(m)).isDefined)
      })
  }

  def prefix: String
  
  def Main: Option[P[AtomicTrigger[_ <: M]]] = None

  def Rule: P[AtomicTrigger[_ <: M]]
}
