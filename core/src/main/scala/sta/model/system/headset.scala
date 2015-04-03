package sta.model.system

import kj.android.common.category
import sta.model.{ Model, ModelCompanion }

trait HeadsetModels {
  @category("headset") sealed abstract class Headset extends Model(Headset)

  implicit object Headset extends ModelCompanion[Headset] {
    case object Connected extends Headset
    case object Disconnected extends Headset
  }
}
