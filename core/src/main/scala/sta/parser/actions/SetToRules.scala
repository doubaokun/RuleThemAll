package sta.parser.actions

import fastparse.noApi._
import scala.{Int => ScalaInt}
import sta.model.actions.{SetSoundTo, SetToSettings, ChangeSoundProfile, SetTo}

object SetToRules extends ActionParser[SetTo] {
  import white._

  def rules: Seq[(String, P[SetTo])] = Seq(
    "brightness" -> Percent.map(SetToSettings.brightness),
    "screen timeout" -> (Duration.map(_.toMillis.toInt) |
      "never".l.map(_ => ScalaInt.MaxValue)).map(SetToSettings.timeout),
    "sound profile" -> mapParser(ChangeSoundProfile.Mode.namesToValuesMap).map(ChangeSoundProfile(_))
  ) ++ SetSoundTo.StreamType.values.map { case st =>
    s"${st.entryName} sound" -> (Percent.map(SetSoundTo.Volume(st, _)) |
      "muted".l.map(_ => SetSoundTo.Muted(st, mute = true)) |
      "unmuted".l.map(_ => SetSoundTo.Muted(st, mute = false)))
  }

  val Rule: P[SetTo] = {
    def single(kv: (String, P[SetTo])): P[SetTo] = {
      val (prefix, value) = kv
      prefix.lWS ~! "to" ~! P(value)(prefix)
    }
    val seq = rules

    "set" ~! seq.tail.foldLeft(single(seq.head)) { _ | single(_) }
  }
}
