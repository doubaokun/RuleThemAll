package sta.parser

import scala.language.implicitConversions
import fastparse.all._
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
import sta.parser.Extras._
import sta.tests.PropertyChecks

@Config(sdk = Array(19), manifest = Config.NONE)
class RulesParserSpec extends FlatSpec with RobolectricSuite with PropertyChecks with Matchers {
  implicit def loadInputStream(is: InputStream): String = io.Source.fromInputStream(is).mkString

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
    intercept[SyntaxError] {
      RulesParser.Multi.parse("rulee{do{set sound profile to silent}}").get
    }

    intercept[SyntaxError] {
      RulesParser.Multi.parse("rule 1e{do{set sound profile to silent}}").get
    }

    intercept[SyntaxError] {
      RulesParser.Multi.parse("rule e{do{setsound profile to silent}}").get
    }

    intercept[SyntaxError] {
      RulesParser.Multi.parse("rule e{do{set soundprofile to silent}}").get
    }

    intercept[SyntaxError] {
      RulesParser.Multi.parse("rule e{do{set sound profileto silent}}").get
    }

    intercept[SyntaxError] {
      RulesParser.Multi.parse("rule e{when()do{set sound profile to silent}}").get
    }

    intercept[SyntaxError] {
      RulesParser.Multi.parse("rule e{when(batteryplugged ac)do{set sound profile to silent}}").get
    }

    intercept[SyntaxError] {
      RulesParser.Multi.parse("rule e{when(battery pluggedac)do{set sound profile to silent}}").get
    }
  }

  it should "parse rule with no conditions" in {
    val expected = Rule(
      name = "e",
      branches = Branch(Seq.empty, Seq.empty) :: Nil,
      actions = Seq(ChangeSoundProfile(ChangeSoundProfile.Mode.Silent))
    ) :: Nil
    val actual = RulesParser.Multi.parse(
      """
        |rule e{do{set sound profile to silent}}
      """.stripMargin).get.value

    compareRules(actual = actual, expected = expected)
  }

  it should "parse scripts with single rule" in {
    val expected = Rule(
      name = "first_rule",
      branches = Seq(
        Branch(conditions = List(
          Trigger.Condition[Battery](_.plugged == Battery.Plugged.withName("ac")),
          Trigger.Condition[Headset](_ == Headset.withName("connected"))
        ), timers = Nil),
        Branch(conditions = List(
          Trigger.Condition[Battery](_.level > ub"50"),
          Trigger.Condition[Headset](_ == Headset.withName("connected"))
        ), timers = Nil)
      ),
      actions = Seq(
        ChangeSoundProfile(ChangeSoundProfile.Mode.Vibrate),
        TurnOnOff.WiFi(enable = true)
      )
    ) :: Nil
    val actual = RulesParser.Multi.parse(getClass.getResourceAsStream("/single.rule")).get.value

    compareRules(actual = actual, expected = expected)
  }

  it should "parse scripts with multiple rules" in {
    val expected = Rule(
      name = "charger_connected",
      branches = Seq(Branch(conditions = List(
        Trigger.Condition[PowerState](_ == PowerState.withName("connected"))
      ), timers = Nil)),
      actions = Seq(SetToSettings.brightness(UByte(100)), SetToSettings.timeout(Int.MaxValue))
    ) ::  Rule(
      name = "charger_disconnected",
      branches = Seq(Branch(conditions = List(
        Trigger.Condition[PowerState](_ == PowerState.withName("disconnected"))
      ), timers = Nil)),
      actions = Seq(SetToSettings.brightness(UByte(60)), SetToSettings.timeout(1.minute.toMillis.toInt))
    ) :: Rule(
      name = "music_player",
      branches = Seq(Branch(conditions = List(
        Trigger.Condition[Headset](_ == Headset.withName("connected")),
        Trigger.Condition[Network](_.state == Network.State.withName("connected"))
      ), timers = Nil)),
      actions = Seq(
        LaunchApplication.UsingAppName("Spotify"),
        SetSoundTo.Volume(SetSoundTo.StreamType.Music, UByte(70)),
        SetSoundTo.Volume(SetSoundTo.StreamType.Ring, UByte(50)),
        SetSoundTo.Muted(SetSoundTo.StreamType.Notification, mute = true)
      )
    ) :: Rule(
      name = "no_music_player",
      branches = Seq(Branch(conditions = List(
        Trigger.Condition[Headset](_ == Headset.withName("disconnected"))
      ), timers = Nil)),
      actions = Seq(
        SetSoundTo.Volume(SetSoundTo.StreamType.Music, UByte(30)),
        SetSoundTo.Volume(SetSoundTo.StreamType.Ring, UByte(100)),
        SetSoundTo.Muted(SetSoundTo.StreamType.Notification, mute = false)
      )
    )  :: Nil
    val actual = RulesParser.Multi.parse(getClass.getResourceAsStream("/multiple.rule")).get.value

    compareRules(actual = actual, expected = expected)
  }

  it should "parse dense script" in {
    val expected = Rule(
      name = "_rul3_dens3_",
      branches = Seq(
        Branch(conditions = List(Trigger.Condition[Battery](_.level <= ub"70")), timers = Nil),
        Branch(conditions = List(Trigger.Condition[Headset](_ == Headset.withName("disconnected"))), timers = Nil)
      ),
      actions = Seq(
        ChangeSoundProfile(ChangeSoundProfile.Mode.Silent),
        TurnOnOff.Bluetooth(enable = false)
      )
    ) :: Nil
    val actual = RulesParser.Multi.parse(
      "rule _rul3_dens3_{when(or(battery level <= 70%,headset disconnected))do{set sound profile to silent;turn bluetooth off}}"
    ).get.value

    compareRules(actual = actual, expected = expected)
  }

  it should "ignore comments" in {
    val expected = Rule(
      name = "dense",
      branches = Seq(
        Branch(conditions = List(
          Trigger.Condition[Headset](_ == Headset.withName("disconnected")),
          Trigger.Condition[Battery](_.level > ub"80"),
          Trigger.Condition[PowerState](_ == PowerState.withName("connected"))
        ), timers = Nil)
      ),
      actions = Seq(
        ChangeSoundProfile(ChangeSoundProfile.Mode.Silent),
        TurnOnOff.WiFi(enable = true),
        TurnOnOff.Bluetooth(enable = false)
      )
    ) :: Nil

    val actual = RulesParser.Multi.parse(
      """// this is a comment
        |rule with_comments// this is another one
        |{
        |    when (
        |        headset
        |            disconnected,
        |        battery(
        |            level > 80%,
        |            power connected
        |        )
        |        //
        |        // network connected,
        |        //time matches "* * * * *"
        |        //
        |    ) do {
        |        set sound profile to silent/**/;turn wifi on
        |        /* and this is a
        |             multiline comment*/
        |        turn bluetooth/*and a multiline comment in an unusual position*/ off/**/
        |    //}
        |}/**/}
      """.stripMargin
    ).get.value

    compareRules(actual = actual, expected = expected)
  }
}
