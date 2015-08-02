package sta.parser.actions

import android.media.AudioManager
import fastparse.noApi._
import scalaz.Tag
import sta.model.actions.{ Action, ChangeSoundProfile }
import sta.parser.WhitespaceSkip

object SoundProfileRules extends SetActionParser with WhitespaceSkip {
  import white._

  protected def ruleObject: String = "sound profile"

  protected def ruleAdverb: P[Action] = P(
    ("normal".! map (_ => ChangeSoundProfile(Tag(AudioManager.RINGER_MODE_NORMAL)))) |
      ("vibrate".! map (_ => ChangeSoundProfile(Tag(AudioManager.RINGER_MODE_VIBRATE)))) |
      ("silent".! map (_ => ChangeSoundProfile(Tag(AudioManager.RINGER_MODE_SILENT))))
  )
}
