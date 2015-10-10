package rta.service

import rta.model.Rule
import rta.model.actions.ActionKind
import scala.collection.mutable
import spire.math.UByte

sealed trait ConflictSetResolution {
  def resolve(conflictSet: TraversableOnce[Rule]): Iterable[Rule]
}

object ConflictSetResolution {
  lazy val default: ConflictSetResolution = new ConflictSetResolution {
    private object KindExtractor {
      def unapply(rule: Rule): Option[Set[ActionKind]] = Some(rule.actions.map(_.kind)(collection.breakOut))
    }

    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.MutableDataStructures"))
    def resolve(conflictSet: TraversableOnce[Rule]): Set[Rule] = {
      val acc = mutable.LinkedHashMap.empty[Set[ActionKind], Set[Rule]]
      conflictSet.foreach {
        case r@KindExtractor(kinds) if !r.executed =>
          val kv = acc.collectFirst {
            case (k, v) if kinds.exists(k.contains) => (k, v + r)
          }.getOrElse(kinds -> Set(r))
          acc -= kv._1 += (kv._1 ++ kinds -> kv._2)
        case _ =>
      }
      acc.map(_._2.foldLeft(UByte.MinValue -> Set.empty[Rule]) {
        case ((max, rules), rule) =>
          if (rule.priority > max) rule.priority -> Set(rule)
          else if (rule.priority == max) max -> (rules + rule)
          else max -> rules
      }._2.maxBy(_.actions.length))(collection.breakOut)
    }
  }
}
