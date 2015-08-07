package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.{Action, ChangeSoundProfile}
import sta.parser.WhitespaceSkip

object SoundProfileRules extends SetActionParser with WhitespaceSkip {
  import white._

  protected def ruleObject: String = "sound profile"

  protected def ruleAdverb: P[Action] = P(
    mapParser(ChangeSoundProfile.Mode.namesToValuesMap) map (v => ChangeSoundProfile(v))
  )
}
