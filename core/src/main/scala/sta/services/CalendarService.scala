package sta.services

import android.content.Intent
import kj.android.common.UsedFeatures
import sta.model.system.Implicits._

// TODO
class CalendarService extends ServiceFragment[CalendarEvent] {
  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_PROVIDER_CHANGED &&
      intent.getData.getScheme  == "content" && intent.getData.getHost == "com.android.calendar" =>
        None
  }

  def reactOn: Set[String] = implicitly[UsedFeatures[CalendarEvent]].intents
}
