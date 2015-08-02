package sta.model.system

import enumeratum.Enum
import kj.android.common.category
import sta.model.{ Model, ModelCompanion, ModelEnumEntry }

trait HeadsetModels {

  @category("headset")
  sealed abstract class Headset extends Model(Headset) with ModelEnumEntry

  implicit object Headset extends ModelCompanion[Headset] with Enum[Headset] {
    lazy val values = findValues

    case object Connected extends Headset
    case object Disconnected extends Headset
  }

}
