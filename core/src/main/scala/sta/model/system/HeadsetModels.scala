package sta.model.system

import android.content.Intent
import enumeratum.Enum
import kj.android.common.{intent, category}
import sta.model._

trait HeadsetModels {

  @category("headset")
  @intent(Intent.ACTION_HEADSET_PLUG)
  sealed abstract class Headset extends Model(Headset) with FromIntEntry

  implicit object Headset extends ModelCompanion[Headset] with Enum[Headset] with FromInt[Headset] {
    lazy val values = findValues

    case object Connected extends Headset {
      def intValue: Int = 1
    }
    case object Disconnected extends Headset {
      def intValue: Int = 0
    }
  }

}
