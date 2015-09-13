package sta.model.triggers

import android.content.{Context, Intent}
import android.provider.CalendarContract
import android.provider.CalendarContract.Instances
import enumeratum.Enum
import java.util.Date
import scala.annotation.tailrec
import scala.concurrent.duration._
import sta.common.{data, category, action}
import sta.model._

trait CalendarModels {

  @category("calendar event")
  @action(Intent.ACTION_PROVIDER_CHANGED)
  @data(CalendarContract.CONTENT_URI)
  case class CalendarEvent(availability: CalendarEvent.Availability,
    title: String, description: String, location: String) extends Model[CalendarEvent](CalendarEvent)

  implicit object CalendarEvent extends ModelCompanion[CalendarEvent] {
    sealed abstract class State(dateColumn: String) extends ModelEnumEntry {
      def find(from: Date, timeWindow: Duration, ctx: Context,
        conditions: Trigger.Condition[_ <: CalendarEvent]*): Option[Date] = {
        import CalendarContract._

        val begin = from.getTime
        val end = begin + timeWindow.toMillis
        val uri = Instances.CONTENT_URI.buildUpon()
          .appendEncodedPath(begin.toString)
          .appendEncodedPath(end.toString)
          .build()
        val projection = Array(EventsColumns.TITLE, EventsColumns.DESCRIPTION,
          EventsColumns.EVENT_LOCATION, EventsColumns.AVAILABILITY, Instances.BEGIN, Instances.END)
        val query = s"( $dateColumn >= ? ) AND ( $dateColumn <= ? )"
        val args = Array((begin - 60.seconds.toMillis).toString, end.toString)
        val order = s"$dateColumn ASC"
        val cursor = ctx.getContentResolver.query(uri, projection, query, args, order)

        @tailrec def getFirst: Option[Date] = {
          if (cursor.moveToNext()) {
            val title = cursor.getString(cursor.getColumnIndex(EventsColumns.TITLE))
            val description = cursor.getString(cursor.getColumnIndex(EventsColumns.DESCRIPTION))
            val location = cursor.getString(cursor.getColumnIndex(EventsColumns.EVENT_LOCATION))
            val availability = Availability.fromInt(cursor.getInt(cursor.getColumnIndex(EventsColumns.AVAILABILITY)))
            val ce = CalendarEvent(availability = availability,
              title = title, description = description, location = location)
            if (conditions.forall(_.satisfiedBy(ce))) Some(new Date(cursor.getLong(cursor.getColumnIndex(dateColumn))))
            else getFirst
          } else None
        }

        try {
          getFirst
        } finally {
          cursor.close()
        }
      }
    }
    object State extends Enum[State] {
      lazy val values = findValues

      case object Starts extends State(Instances.BEGIN)
      case object Ends extends State(Instances.END)
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
