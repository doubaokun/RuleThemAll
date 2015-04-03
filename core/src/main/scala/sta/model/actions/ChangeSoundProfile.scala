package sta.model.actions

import android.content.Context
import android.media.AudioManager
import kj.android.common.category

import scalaz.{ @@, Tag }

trait RingerMode

@category("sound")
case class ChangeSoundProfile(mode: Int @@ RingerMode) extends Action {
  def execute()(implicit ctx: Context): Unit = {
    ctx.getSystemService(Context.AUDIO_SERVICE).asInstanceOf[AudioManager].setRingerMode(Tag.unwrap(mode))
  }
}
