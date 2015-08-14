package sta.services

import android.content.Intent
import sta.model.system._

// TODO
class CalendarService extends ServiceFragment[CalendarEvent] {
  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_PROVIDER_CHANGED &&
      intent.getData.getScheme  == "content" && intent.getData.getHost == "com.android.calendar" =>
        None
  }

  protected[sta] def reactOn: Set[String] = Set(
    Intent.ACTION_PROVIDER_CHANGED
  )
}
