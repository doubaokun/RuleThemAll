package sta.model.actions

import android.content.Context
import android.media.AudioManager
import enumeratum.Enum
import kj.android.logging.Logger
import sta.model.{FromInt, FromIntEntry}


case class ChangeSoundProfile(mode: ChangeSoundProfile.Mode) extends Action {
  def execute()(implicit ctx: Context): Unit = {
    val value = mode.intValue
    val manager = ctx.getSystemService(Context.AUDIO_SERVICE).asInstanceOf[AudioManager]
    if (manager.getRingerMode != value) manager.setRingerMode(value)
  }
}

object ChangeSoundProfile {
  sealed abstract class Mode extends FromIntEntry
  object Mode extends Enum[Mode] {
    lazy val values = findValues

    case object Silent extends Mode {
      def intValue: Int = AudioManager.RINGER_MODE_SILENT
    }
    case object Vibrate extends Mode {
      def intValue: Int = AudioManager.RINGER_MODE_VIBRATE
    }
    case object Normal extends Mode {
      def intValue: Int = AudioManager.RINGER_MODE_NORMAL
    }
  }
}
