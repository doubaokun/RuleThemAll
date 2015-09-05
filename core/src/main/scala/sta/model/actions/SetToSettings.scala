package sta.model.actions

import android.content.Context
import android.provider.Settings

case class SetToSettings() extends SetTo {
  def execute()(implicit ctx: Context): Unit = Settings.System.putInt(cResolver,
    Settings.System.SCREEN_BRIGHTNESS_MODE, Settings.System.SCREEN_BRIGHTNESS_MODE_MANUAL)
}
