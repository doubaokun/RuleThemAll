package sta.services

import android.content.Intent
import sta.model.system.Headset

@genReactOn
abstract class HeadsetService extends ServiceFragment[Headset] {
  final def handle(intent: Intent) = intent.getAction match {
    case Intent.ACTION_HEADSET_PLUG => Headset.fromInt(intent.extra[Int].state)
  }
}
