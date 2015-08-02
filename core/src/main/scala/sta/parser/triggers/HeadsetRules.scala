package sta.parser.triggers

import scala.language.implicitConversions

import fastparse.noApi._
import kj.android.common.UsedFeatures
import sta.model.system._
import sta.model.triggers.AtomicTrigger
import sta.parser.WhitespaceSkip

object HeadsetRules extends TriggerParser[Headset] with WhitespaceSkip {
  import white._

  type M = Headset

  def prefix: String = implicitly[UsedFeatures[Headset]].category

  private def connectivity: P[AtomicTrigger[Headset]] = P(
    mapParser(Headset.namesToValuesMap) map (v => AtomicTrigger[Headset](_ == v))
  )

  val Main: P[AtomicTrigger[_ <: Headset]] = connectivity
}
