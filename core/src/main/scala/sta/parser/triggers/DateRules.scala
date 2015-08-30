package sta.parser.triggers

import fastparse.noApi._
import java.text.SimpleDateFormat
import kj.android.concurrent.Task
import scala.concurrent.duration._
import scala.util.Try
import sta.common.Uses
import sta.model.triggers.Implicits._
import sta.model.triggers.Condition

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
      case (p, d) => Condition.Trigger[DateTime](dt => p.format(dt.date) == d)
    }
  }

  val Rule: P[Condition.Standalone[_ <: DateTime]] = date
}
