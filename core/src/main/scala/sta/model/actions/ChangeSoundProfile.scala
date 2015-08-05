package sta.model.actions

import android.content.Context
import android.media.AudioManager
import enumeratum.Enum
import kj.android.common.category
import sta.model.{FromInt, FromIntEntry}

@category("sound")
case class ChangeSoundProfile(mode: ChangeSoundProfile.Mode) extends Action {
  def execute()(implicit ctx: Context): Unit = {
    ctx.getSystemService(Context.AUDIO_SERVICE).asInstanceOf[AudioManager].setRingerMode(mode.intValue)
  }
}

object ChangeSoundProfile {
  sealed abstract class Mode extends FromIntEntry
  object Mode extends Enum[Mode] with FromInt[Mode] {
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
