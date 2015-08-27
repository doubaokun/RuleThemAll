package sta.parser.triggers

import fastparse.noApi._
import java.text.SimpleDateFormat
import scala.util.Try
import sta.common.{Requirement, Uses}
import sta.model.triggers.ModelTrigger
import sta.model.triggers.Implicits._

object DateRules extends TriggerParser[DateTime] {
  import white._

  def Prefix: String = Uses.categoryOf[DateTime]

  private object Pattern {
    def unapply(pattern: String): Option[SimpleDateFormat] =
      Try(new SimpleDateFormat(pattern)).toOption
  }

  private def date = {
    val pattern = for {
      Pattern(p) <- SingleLineString
    } yield p
    ("with" ~ "format" ~ pattern).?.map(_.getOrElse(new SimpleDateFormat())) ~ "is" ~ SingleLineString map {
      case (p, d) => ModelTrigger[DateTime](dt => p.format(dt.date) == d).withRequirements(Requirement.DateBased)
    }
  }

  val Rule: P[ModelTrigger[_ <: DateTime]] = date
}
