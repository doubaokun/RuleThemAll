package rta.service

import rta.common.Uses
import rta.model.triggers.Implicits._

class HeadsetService extends ServiceFragment[Headset] {
  final val handle: PF = {
    case intent if intent.getAction == Uses.actionFor[Headset] =>
      Headset.fromInt(intent[Int]("state"))
  }
}
