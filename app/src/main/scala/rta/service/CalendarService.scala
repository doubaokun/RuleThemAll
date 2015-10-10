package rta.service

import android.content.Intent
import rta.common.Uses
import rta.model.triggers.Implicits._

class CalendarService(root: RulesExecutor) extends ServiceFragment[CalendarEvent] {
  final val handle: PF = {
    case intent if intent.getAction == Uses.actionFor[CalendarEvent] &&
      intent.getData == Uses.dataOf[CalendarEvent] =>
      root.resetTimers(implicitly[Uses[CalendarEvent]].requirements)
      None
  }
}
