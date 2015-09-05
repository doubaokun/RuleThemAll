package sta.parser.actions

import fastparse.noApi._
import scala.{Int => ScalaInt}
import sta.model.actions.{SetToSettings, ChangeSoundProfile, SetTo}

object SetToRules extends ActionParser[SetTo] {
  import white._

  def rules: Seq[(String, P[SetTo])] = Seq(
    "brightness" -> Percent.map(SetToSettings.brightness),
    "screen timeout" -> (Duration.map(_.toMillis.toInt) |
      "never".u.map(_ => ScalaInt.MaxValue)).map(SetToSettings.timeout),
    "sound profile" -> mapParser(ChangeSoundProfile.Mode.namesToValuesMap).map(ChangeSoundProfile(_))
  )

  val Rule: P[SetTo] = {
    def single(kv: (String, P[SetTo])): P[SetTo] = {
      val (str, value) = kv
      val splitted = str.split("\\s+")
      val prefix = splitted.tail.foldLeft(splitted.head: P[Unit]) { _ ~ _ }
      prefix ~! "to" ~! P(value)(str)
    }
    val seq = rules

    "set" ~! seq.tail.foldLeft(single(seq.head)) { _ | single(_) }
  }
}
