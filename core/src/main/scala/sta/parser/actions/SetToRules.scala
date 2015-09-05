package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.{ChangeSoundProfile, SetTo}

object SetToRules extends ActionParser[SetTo] {
  import white._

  def rules: Seq[(String, P[SetTo])] = Seq(
    "sound profile" ->  P(
      mapParser(ChangeSoundProfile.Mode.namesToValuesMap) map (ChangeSoundProfile(_))
    )
  )

  val Rule: P[SetTo] = {
    def single(kv: (String, P[SetTo])): P[SetTo] = {
      val (str, value) = kv
      val splitted = str.split("\\s+")
      val prefix = splitted.tail.foldLeft(splitted.head: P[Unit]) { _ ~ _ }
      prefix ~ "to" ~ value
    }
    val seq = rules

    "set" ~ seq.tail.foldLeft(single(seq.head)) { _ | single(_) }
  }
}
