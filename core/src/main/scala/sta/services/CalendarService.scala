package sta.services

import android.content.{Context, Intent}
import android.provider.CalendarContract
import kj.android.concurrent.Task
import sta.common.Uses
import sta.model.triggers.Implicits._
import scala.concurrent.duration._

class CalendarService(root: STAService) extends ServiceFragment[CalendarEvent] {
  import CalendarEvent._
  import CalendarContract._

  private[this] val updateTask = Task.schedule(0.seconds, 24.hours) {
    fetchCalendarEvents()
  }

  private def fetchCalendarEvents(): Unit = {
//    val from = System.currentTimeMillis() - 1.minute.toMillis
//    val to = from + 24.hours.toMillis
//    val columns = Array(EventsColumns.TITLE, EventsColumns.DESCRIPTION,
//      EventsColumns.EVENT_LOCATION, EventsColumns.AVAILABILITY, Instances.BEGIN, Instances.END)
//    val cursor = Instances.query(root.getContentResolver, columns, from, to)
//
//    while(cursor.moveToNext()) {
//      val title = cursor.getString(cursor.getColumnIndex(EventsColumns.TITLE))
//      val description = cursor.getString(cursor.getColumnIndex(EventsColumns.DESCRIPTION))
//      val location = cursor.getString(cursor.getColumnIndex(EventsColumns.EVENT_LOCATION))
//      val availability = Availability.fromInt(cursor.getInt(cursor.getColumnIndex(EventsColumns.AVAILABILITY)))
//      val ce = CalendarEvent(availability = availability,
//        title = title, description = description, location = location) _
//      Task.scheduleOnce((System.currentTimeMillis() +
//        cursor.getLong(cursor.getColumnIndex(Instances.BEGIN))).millis) {
//        root.updateState { state =>
//          val multi = state.get(Key).getOrElse(MultiModel.empty[CalendarEvent])
//          state + (Key -> (multi + ce(State.Starts)))
//        }
//      }
//      Task.scheduleOnce((System.currentTimeMillis() +
//        cursor.getLong(cursor.getColumnIndex(Instances.END))).millis) {
//        root.updateState { state =>
//          val multi = state.get(Key).getOrElse(MultiModel.empty[CalendarEvent])
//          state + (Key -> (multi + ce(State.Ends)))
//        }
//      }
//    }
  }

  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_PROVIDER_CHANGED &&
      intent.getData == Uses.dataOf[CalendarEvent] =>
      updateTask.cancel(false)
      fetchCalendarEvents()
      updateTask.run(_ => ())
      None
  }
}
