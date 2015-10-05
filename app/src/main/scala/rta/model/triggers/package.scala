package rta.model

import scala.language.implicitConversions
import shapeless.HMap

package object triggers {
  private[triggers] implicit def liftToState[M <: BaseModel](model: M): HMap[ModelKV] = {
    import model.companion._
    HMap.apply[ModelKV](Key -> model.lift)(model.ev)
  }

  object Implicits extends BatteryModels with BluetoothModels with CalendarModels with
    HeadsetModels with NetworkModels with WiFiModels
}
