package rta.model.actions

import android.content.Context
import android.provider.Settings
import spire.math.UByte

final case class SetToSettings(settings: (String, Int)*) extends SetTo {
  def execute()(implicit ctx: Context): Unit =
    settings.foreach { case (name, value) =>
      Settings.System.putInt(ctx.getContentResolver, name, value)
    }

  override def kind: ActionKind = ActionKind(settings.map(_._1)(collection.breakOut): Set[String])
}

object SetToSettings {
  def brightness(percent: UByte) = {
    SetToSettings(
      Settings.System.SCREEN_BRIGHTNESS_MODE -> Settings.System.SCREEN_BRIGHTNESS_MODE_MANUAL,
      Settings.System.SCREEN_BRIGHTNESS -> (percent.toFloat * 255 / 100).toInt
    )
  }

  def timeout(value: Int) =  {
    SetToSettings(Settings.System.SCREEN_OFF_TIMEOUT -> value)
  }
}
