package sta.model.actions

import android.content.Context
import android.media.AudioManager
import enumeratum.Enum
import kj.android.common.SystemServices._
import sta.model.FromIntEntry

case class ChangeSoundProfile(mode: ChangeSoundProfile.Mode) extends Action {
  def execute()(implicit ctx: Context): Unit = {
    val value = mode.intValue
    val manager = audioManger
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
