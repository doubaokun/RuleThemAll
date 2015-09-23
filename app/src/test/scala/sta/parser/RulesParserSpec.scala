package sta.parser

import scala.language.implicitConversions
import fastparse.core.SyntaxError
import java.io.InputStream
import org.robolectric.annotation.Config
import org.scalatest.{FlatSpec, Matchers, RobolectricSuite}
import scala.concurrent.duration._
import spire.implicits._
import spire.math.UByte
import sta.common.Uses._
import sta.model.Rule
import sta.model.actions._
import sta.model.triggers.Implicits._
import sta.model.triggers.Trigger.Branch
import sta.model.triggers._
import sta.tests.PropertyChecks

@Config(sdk = Array(19), manifest = Config.NONE)
class RulesParserSpec extends FlatSpec with RobolectricSuite with PropertyChecks with Matchers {
  implicit def loadInputStream(is: InputStream): String = io.Source.fromInputStream(is).mkString

  implicit class ParserResult(result: Either[SyntaxError, Seq[Rule]]) {
    def get = result match {
      case Left(th) => sys.error(th.toString())
      case Right(seq) => seq
    }
  }
  
  def compareRules(actual: Seq[Rule], expected: Seq[Rule]): Unit = {
    actual.size should === (expected.size)
    actual.zip(expected).foreach { case (actualRule, expectedRule) =>
      actualRule.actions should contain theSameElementsAs expectedRule.actions
      actualRule.branches.size should === (expectedRule.branches.size)
      actualRule.branches.zip(expectedRule.branches).foreach { case (actualBranch, expectedBranch) =>
        actualBranch.timers should contain theSameElementsAs expectedBranch.timers
        actualBranch.conditions should contain theSameElementsAs expectedBranch.conditions
      }
    }  
  }

  behavior of "RulesParser"

  it should "return error on malformed input" in {
    RulesParser.parse("rulee{do{set sound profile to silent}}").isLeft should === (true)

    RulesParser.parse("rule e_{do{set sound profile to silent}}").isLeft should === (true)

    RulesParser.parse("rule _e{do{set sound profile to silent}}").isLeft should === (true)

    RulesParser.parse("rule e_1_{do{set sound profile to silent}}").isLeft should === (true)

    RulesParser.parse("rule 1e{do{set sound profile to silent}}").isLeft should === (true)

    RulesParser.parse("rule e{do{setsound profile to silent}}").isLeft should === (true)

    RulesParser.parse("rule e{do{set soundprofile to silent}}").isLeft should === (true)

    RulesParser.parse("rule e{do{set sound profileto silent}}").isLeft should === (true)

    RulesParser.parse("rule e{when()do{set sound profile to silent}}").isLeft should === (true)

    RulesParser.parse("rule e{when(batteryplugged ac)do{set sound profile to silent}}").isLeft should === (true)

    RulesParser.parse("rule e{when(battery pluggedac)do{set sound profile to silent}}").isLeft should === (true)
  }

  it should "parse rule with no conditions" in {
    val expected = Rule(
      name = "empty",
      branches = Branch(Seq.empty, Seq.empty) :: Nil,
      actions = Seq(ChangeSoundProfile(ChangeSoundProfile.Mode.Silent))
    ) :: Nil
    val actual = RulesParser.parse(
      """
        |rule empty{do{set sound profile to silent}}
      """.stripMargin)

    compareRules(actual = actual.get, expected = expected)
    actual.get.size should === (1)
  }

  it should "parse dense script" in {
    val expected = Rule(
      name = "dense",
      branches = Seq(
        Branch(conditions = List(Trigger.Condition[Battery](_.level <= ub"70"))),
        Branch(conditions = List(Trigger.Condition[Headset](_ == Headset.withName("disconnected"))))
      ),
      actions = Seq(
        ChangeSoundProfile(ChangeSoundProfile.Mode.Silent),
        TurnOnOffDevice.Bluetooth(enable = false)
      )
    ) :: Nil
    val actual = RulesParser.parse(
      "rule dense{when(or(battery level <= 70%,headset disconnected))do{set sound profile to silent;turn bluetooth off}}"
    )

    compareRules(actual = actual.get, expected = expected)
  }

  it should "parse scripts with single rule" in {
    val expected = Rule(
      name = "first_rule",
      branches = Seq(
        Branch(List(
          Trigger.Condition[Battery](_.plugged == Battery.Plugged.withName("ac")),
          Trigger.Condition[Headset](_ == Headset.withName("connected"))
        )),
        Branch(List(
          Trigger.Condition[Battery](_.level > ub"50"),
          Trigger.Condition[Headset](_ == Headset.withName("connected"))
        ))
      ),
      actions = Seq(
        ChangeSoundProfile(ChangeSoundProfile.Mode.Vibrate),
        TurnOnOffDevice.WiFi(enable = true)
      )
    ) :: Nil
    val actual = RulesParser.parse(getClass.getResourceAsStream("/single.rule"))

    compareRules(actual = actual.get, expected = expected)
  }

  it should "parse scripts with multiple rules" in {
    val expected = Rule(
      name = "charger_connected",
      branches = Seq(Branch(List(Trigger.Condition[PowerState](_ == PowerState.withName("connected"))))),
      actions = Seq(SetToSettings.brightness(UByte(100)), SetToSettings.timeout(Int.MaxValue))
    ) ::  Rule(
      name = "charger_disconnected",
      branches = Seq(Branch(List(Trigger.Condition[PowerState](_ == PowerState.withName("disconnected"))))),
      actions = Seq(SetToSettings.brightness(UByte(60)), SetToSettings.timeout(1.minute.toMillis.toInt))
    ) :: Rule(
      name = "music_player",
      branches = Seq(Branch(List(
        Trigger.Condition[Headset](_ == Headset.withName("connected")),
        Trigger.Condition[Network](_.state == Network.State.withName("connected"))
      ))),
      actions = Seq(
        LaunchApplication.UsingAppName("Spotify"),
        SetSoundTo.Volume(SetSoundTo.StreamType.Music, UByte(70)),
        SetSoundTo.Volume(SetSoundTo.StreamType.Ring, UByte(50)),
        SetSoundTo.Muted(SetSoundTo.StreamType.Notification, mute = true)
      )
    ) :: Rule(
      name = "no_music_player",
      branches = Seq(Branch(List(
        Trigger.Condition[Headset](_ == Headset.withName("disconnected"))
      ))),
      actions = Seq(
        SetSoundTo.Volume(SetSoundTo.StreamType.Music, UByte(30)),
        SetSoundTo.Volume(SetSoundTo.StreamType.Ring, UByte(100)),
        SetSoundTo.Muted(SetSoundTo.StreamType.Notification, mute = false)
      )
    )  :: Nil
    val actual = RulesParser.parse(getClass.getResourceAsStream("/multiple.rule"))

    compareRules(actual = actual.get, expected = expected)
  }
}
