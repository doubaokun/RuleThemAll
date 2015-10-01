package sta.common

import android.content.Intent

sealed abstract class Requirement extends Serializable

object Requirement {
  final case class IntentBased(private val intent: Intent) extends Requirement {
    override def hashCode(): Int = intent.filterHashCode()

    override def equals(o: Any): Boolean = o match {
      case IntentBased(other) => intent.filterEquals(other)
      case _ => false
    }
  }
}
