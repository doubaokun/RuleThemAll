package sta.parser.triggers

import scala.language.implicitConversions

import fastparse.all._
import sta.model.Model
import sta.model.triggers.AtomicTrigger
import sta.parser.BasicRules

trait TriggerParser[M <: Model] extends BasicRules {
  def prefix: String

  def Main: P[AtomicTrigger[_ <: M]]

  def mapParser[T](map: Map[String, T]): P[T] = {
    def makeRule(kv: (String, T)): P[T] = kv._1.! map (_ => kv._2)

    if (map.isEmpty) Fail
    else map.tail.foldLeft(makeRule(map.head)) {
      case (r, kv) =>
        r | makeRule(kv)
    }
  }
}
