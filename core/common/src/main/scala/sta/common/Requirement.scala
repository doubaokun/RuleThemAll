package sta.common

import android.content.Intent

sealed abstract class Requirement

object Requirement {

  class IntentBased(private val intent: Intent) extends Requirement {
    override final def hashCode(): Int = intent.filterHashCode()

    override final def equals(o: Any): Boolean = o match {
      case other: IntentBased => intent.filterEquals(other.intent)
      case _ => false
    }
  }

  object IntentBased {
    def apply(intent: Intent): IntentBased = new IntentBased(intent)

    def unapply(requirement: Requirement): Option[Intent] = requirement match {
      case req: IntentBased => Option(req.intent)
      case _ => None
    }
  }

  object DateBased extends Requirement

}
