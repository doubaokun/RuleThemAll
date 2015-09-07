package sta.model

import scala.language.implicitConversions
import shapeless.HMap

package object triggers {
  private[triggers] implicit def liftToState[M <: Model](model: M)
    (implicit companion: ModelCompanion[M]): HMap[ModelKV] = {
    import companion._
    HMap.apply[ModelKV](Key -> model)
  }

  object Implicits extends BatteryModels with BluetoothModels with CalendarModels with
    HeadsetModels with WiFiModels
}
