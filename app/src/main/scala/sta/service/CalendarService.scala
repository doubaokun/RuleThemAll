package sta.service

import android.content.Intent
import sta.common.Uses
import sta.model.triggers.Implicits._

class CalendarService(root: RulesExecutor) extends ServiceFragment[CalendarEvent] {
  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_PROVIDER_CHANGED &&
      intent.getData == Uses.dataOf[CalendarEvent] =>
      root.resetTimers(implicitly[Uses[CalendarEvent]].requirements)
      None
  }
}
