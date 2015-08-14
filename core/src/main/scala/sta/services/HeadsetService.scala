package sta.services

import android.content.Intent
import sta.model.system.Headset

class HeadsetService extends ServiceFragment[Headset] {
  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_HEADSET_PLUG =>
      Headset.fromInt(intent.extra[Int].get("state"))
  }

  protected[sta] def reactOn: Set[String] = Set(
    Intent.ACTION_HEADSET_PLUG
  )
}
