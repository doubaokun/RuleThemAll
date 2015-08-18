package sta.services

import android.content.Intent
import sta.model.triggers.Implicits._

class HeadsetService extends ServiceFragment[Headset] {
  final val handle: PF = {
    case intent if intent.getAction == Intent.ACTION_HEADSET_PLUG =>
      Headset.fromInt(intent.extra[Int]("state"))
  }
}
