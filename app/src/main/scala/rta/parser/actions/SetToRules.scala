package rta.parser.actions

import fastparse.noApi._
import scala.{Int => ScalaInt}
import rta.model.actions._
import rta.parser.ActionParser

object SetToRules extends ActionParser[SetTo] {
  import white._

  def rules: Seq[(String, P[SetTo])] = Seq(
    "location" -> mapParser(SetLocationTo.Mode.namesToValuesMap).map(SetLocationTo(_)),
    "screen brightness" -> Percent.map(SetToSettings.brightness),
    "screen timeout" -> (Duration.map(_.toMillis.toInt) |
      "never".push(ScalaInt.MaxValue)).map(SetToSettings.timeout),
    "sound profile" -> mapParser(ChangeSoundProfile.Mode.namesToValuesMap).map(ChangeSoundProfile(_))
  ) ++ SetSoundTo.StreamType.values.map { case st =>
    s"${st.entryName} sound" -> (Percent.map(SetSoundTo.Volume(st, _)) |
      "muted".push(SetSoundTo.Muted(st, mute = true)) |
      "unmuted".push(SetSoundTo.Muted(st, mute = false)))
  }

  def Rule: P[SetTo] = {
    def single(kv: (String, P[SetTo])): P[SetTo] = {
      val (prefix, value) = kv
      prefix.splitWS.withWS ~! "to" ~! P(value)(prefix)
    }
    val seq = rules

    "set".withWS ~! seq.tail.foldLeft(single(seq.head)) { _ | single(_) }
  }
}
