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
    mapParser(ChangeSoundProfile.Mode.namesToValuesMap) map (v => ChangeSoundProfile(v))
  )
}
