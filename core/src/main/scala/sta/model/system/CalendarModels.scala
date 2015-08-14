package sta.model.system

import android.provider.CalendarContract
import enumeratum.Enum
import kj.android.common.category
import sta.model._

trait CalendarModels {

  @category("calendar_event")
  case class CalendarEvent(state: CalendarEvent.State, name: String,
    description: String, availability: CalendarEvent.Availability) extends Model(CalendarEvent)

  implicit object CalendarEvent extends ModelCompanion[CalendarEvent] {
    sealed abstract class State extends ModelEnumEntry
    object State extends Enum[State] {
      lazy val values = findValues

      case object Starts extends State
      case object Ends extends State
    }

    sealed abstract class Availability extends FromIntEntry
    object Availability extends Enum[Availability] with FromInt[Availability] {
      lazy val values = findValues

      case object Busy extends Availability {
        def intValue: Int = CalendarContract.EventsColumns.AVAILABILITY_BUSY
      }
      case object Free extends Availability {
        def intValue: Int = CalendarContract.EventsColumns.AVAILABILITY_FREE
      }
      case object Tentative extends Availability {
        def intValue: Int = CalendarContract.EventsColumns.AVAILABILITY_TENTATIVE
      }
    }
  }

}