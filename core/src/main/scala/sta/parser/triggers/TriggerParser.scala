package sta.parser.triggers

import scala.language.implicitConversions

import fastparse.all._
import sta.model.Model
import sta.model.triggers.AtomicTrigger
import sta.parser.BasicRules

trait TriggerParser[M <: Model] extends BasicRules {
  def prefix: String

  def Main: P[AtomicTrigger[_ <: M]]
}
