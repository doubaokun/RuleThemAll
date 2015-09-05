package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.{Action, ChangeSoundProfile}
import sta.parser.WhitespaceSkip

object SoundProfileRules extends SetActionParser[ChangeSoundProfile] {
  import white._

  protected def ruleObject: String = "sound profile"

  protected def ruleAdverb: P[ChangeSoundProfile] = P(
    mapParser(ChangeSoundProfile.Mode.namesToValuesMap) map (ChangeSoundProfile(_))
  )
}
