package rta.service

import org.scalatest.{FlatSpec, Matchers}
import rta.model.Rule
import rta.model.actions._
import spire.syntax.literals._

class ConflictSetResolutionSpec extends FlatSpec with Matchers {
  trait Data {
    val rule1a = Rule("rule1a", ub"0", branches = Seq.empty,
      actions = Seq(ChangeSoundProfile(ChangeSoundProfile.Mode.Silent)))
    val rule1b = Rule("rule1b", ub"0", branches = Seq.empty,
      actions = Seq(ChangeSoundProfile(ChangeSoundProfile.Mode.Normal)))
    val rule1c = Rule("rule1b", ub"100", branches = Seq.empty,
      actions = Seq(ChangeSoundProfile(ChangeSoundProfile.Mode.Normal)))

    val rule2a = Rule("rule2a", ub"100", branches = Seq.empty,
      actions = Seq(TurnOnOff.AirplaneMode(true), TurnOnOff.WiFi(false)))
    val rule2b = Rule("rule2b", ub"100", branches = Seq.empty,
      actions = Seq(TurnOnOff.AirplaneMode(true), TurnOnOff.WiFi(false), TurnOnOff.NFC(true)))

    val rule3a = Rule("rule3a", ub"100", branches = Seq.empty,
      actions = Seq(ChangeSoundProfile(ChangeSoundProfile.Mode.Silent))
    )
    val rule3b = Rule("rule3b", ub"100", branches = Seq.empty,
      actions = Seq(ChangeSoundProfile(ChangeSoundProfile.Mode.Silent), TurnOnOff.AirplaneMode(true))
    )
    val rule3c = Rule("rule3c", ub"100", branches = Seq.empty,
      actions = Seq(TurnOnOff.WiFi(true), TurnOnOff.Bluetooth(false), TurnOnOff.AirplaneMode(true))
    )

    val rule4a = Rule("rule4a", ub"100", branches = Seq.empty,
      actions = Seq(SetToSettings.brightness(ub"100"))
    )
    val rule4b = Rule("rule4b", ub"100", branches = Seq.empty,
      actions = Seq(SetToSettings.brightness(ub"50"))
    )
    val rule4c = Rule("rule4c", ub"100", branches = Seq.empty,
      actions = Seq(SetToSettings.timeout(30))
    )
    val rule4d = Rule("rule4d", ub"100", branches = Seq.empty,
      actions = Seq(AlterApplication.Launch("aaa", (_, _) => "pkg.aaa"))
    )
    val rule4e = Rule("rule4e", ub"100", branches = Seq.empty,
      actions = Seq(TurnOnOff.WiFi(false), AlterApplication.Launch("pkg.aaa", (_, _) => "pkg.aaa"))
    )
  }

  "ConflictSetResolution.default" should "resolve various" in new Data {
    // first wins
    ConflictSetResolution.default.resolve(Iterator(rule1a, rule1b)) should
      contain theSameElementsAs Set(rule1a)

    // priority wins
    ConflictSetResolution.default.resolve(Iterator(rule1a, rule1b, rule1c)) should
      contain theSameElementsAs Set(rule1a)

    // more actions wins
    ConflictSetResolution.default.resolve(Iterator(rule2a, rule2b)) should
      contain theSameElementsAs Set(rule2b)

    // more actions wins and conflicts are transitive
    ConflictSetResolution.default.resolve(Iterator(rule3a, rule3b, rule3c)) should
      contain theSameElementsAs Set(rule3c)

    // test tricky actions
    ConflictSetResolution.default.resolve(Iterator(rule4a, rule4b, rule4c, rule4d, rule4e)) should
      contain theSameElementsAs Set(rule4a, rule4c, rule4e)
  }
}
