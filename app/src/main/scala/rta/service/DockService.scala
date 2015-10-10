package rta.service

import android.content.Intent
import rta.common.Uses
import rta.model.triggers.Implicits._

class DockService extends ServiceFragment[DockState] {
  final val handle: PF = {
    case intent if intent.getAction == Uses.actionFor[DockState] =>
      DockState.fromInt(intent[Int](Intent.EXTRA_DOCK_STATE))
  }
}
