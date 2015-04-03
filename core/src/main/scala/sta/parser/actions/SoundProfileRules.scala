package sta.parser.actions

import android.media.AudioManager
import org.parboiled2.Rule1
import sta.model.actions.{ Action, ChangeSoundProfile, RingerMode }

import scalaz.Tag

trait SoundProfileRules extends SetActionParserPart { this: ActionParser ⇒
  protected def ruleObject: String = "sound profile"

  protected def ruleAdverb: Rule1[Action] = rule(
    (endWS("normal") ~ push(ChangeSoundProfile(Tag(AudioManager.RINGER_MODE_NORMAL)))) |
      (endWS("vibrate") ~ push(ChangeSoundProfile(Tag(AudioManager.RINGER_MODE_VIBRATE)))) |
      (endWS("silent") ~ push(ChangeSoundProfile(Tag(AudioManager.RINGER_MODE_SILENT))))
  )
}
