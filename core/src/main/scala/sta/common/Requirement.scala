package sta.common

import android.content.Intent
import android.os.{Parcel, Parcelable}
import scala.annotation.compileTimeOnly

sealed abstract class Requirement extends Parcelable

object Requirement {
  @compileTimeOnly("This field is dummy and should not be accessed.")
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Null"))
  val CREATOR: Parcelable.Creator[Requirement] = null

  class IntentBased(private val intent: Intent) extends Requirement {
    override final def hashCode(): Int = intent.filterHashCode()

    override final def equals(o: Any): Boolean = o match {
      case other: IntentBased => intent.filterEquals(other.intent)
      case _ => false
    }

    def describeContents(): Int = 0

    def writeToParcel(dest: Parcel, flags: Int): Unit = dest.writeParcelable(intent, flags)
  }

  object IntentBased {
    def apply(intent: Intent): IntentBased = new IntentBased(intent)

    def unapply(requirement: Requirement): Option[Intent] = requirement match {
      case req: IntentBased => Option(req.intent)
      case _ => None
    }

    lazy val CREATOR: Parcelable.Creator[IntentBased] = new Parcelable.Creator[IntentBased] {
      def createFromParcel(source: Parcel): IntentBased =
        new IntentBased(source.readParcelable(classOf[Intent].getClassLoader))

      def newArray(size: Int): Array[IntentBased] = new Array[IntentBased](size)
    }
  }

}
