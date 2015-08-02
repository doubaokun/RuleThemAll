package sta.services

import android.content.Intent
import sta.model.system.Headset

@genReactOn
abstract class HeadsetService extends ServiceFragment[Headset] {
  final def handle(intent: Intent) = intent.getAction match {
    case Intent.ACTION_HEADSET_PLUG if intent.extra[Int].state == 1 => Headset.Connected
    case Intent.ACTION_HEADSET_PLUG if intent.extra[Int].state == 0 => Headset.Disconnected
  }
}
