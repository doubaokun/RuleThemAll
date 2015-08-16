package sta.services

import android.content.Intent
import kj.android.common.UsedFeatures
import sta.model.triggers.Implicits._

class HeadsetService extends ServiceFragment[Headset] {
  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_HEADSET_PLUG =>
      Headset.fromInt(intent.extra[Int].get("state"))
  }

  def reactOn: Set[String] = implicitly[UsedFeatures[Headset]].intents
}
