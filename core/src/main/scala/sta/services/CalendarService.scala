package sta.services

import android.content.{Context, Intent}
import android.provider.CalendarContract
import sta.common.Uses
import sta.model.triggers.Implicits._

// TODO
class CalendarService(root: STAService) extends ServiceFragment[CalendarEvent] {
  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_PROVIDER_CHANGED &&
      intent.getData == Uses.dataOf[CalendarEvent] =>
//        val uri = UsedFeatures.dataOf[CalendarEvent]()
      val cursor = CalendarContract.Instances.query(root.getContentResolver, ???, ???, ???)

      None
  }
}
