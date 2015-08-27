package sta.model.triggers

import java.util.Date
import sta.common.category
import sta.model.{Model, ModelCompanion}

trait DateModels {
  @category("date")
  case class DateTime(date: Date) extends Model(DateTime)

  implicit object DateTime extends ModelCompanion[DateTime]
}
