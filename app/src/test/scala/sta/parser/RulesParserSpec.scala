package sta.parser

import scala.language.implicitConversions
import java.io.InputStream
import org.robolectric.annotation.Config
import org.scalatest.{RobolectricSuite, FlatSpec, Matchers}
import spire.implicits._
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

  behavior of "RulesParser"

  it should "parse whole scripts" in {
    val expected = Vector(
      Rule(
        "simple1",
        Seq(
          Branch(conditions = List(
            Trigger.Condition[Battery](_.plugged == Battery.Plugged.withName("ac")),
            Trigger.Condition[Headset](_ == Headset.withName("connected"))
          )),
          Branch(conditions = List(
            Trigger.Condition[Battery](_.level > ub"50"),
            Trigger.Condition[Headset](_ == Headset.withName("connected"))
          ))
        ),
        Vector(
          ChangeSoundProfile(ChangeSoundProfile.Mode.Vibrate),
          TurnOnOffDevice.WiFi(enable = true)
        )
      ),
      Rule(
        "simple2",
        Seq(
          Branch(
            timers = Seq.empty,
            conditions = List(
              Trigger.Condition[Battery](_.level <= ub"70")
            )
          ),
          Branch(
            timers = Seq.empty,
            conditions = List(
              Trigger.Condition[Headset](_ == Headset.withName("disconnected"))
            )
          )
        ),
        Vector(
          ChangeSoundProfile(ChangeSoundProfile.Mode.Silent),
          TurnOnOffDevice.Bluetooth(enable = false)
        )
      )
    )

    val actual = RulesParser.parse(getClass.getResourceAsStream("/simple"))

    actual.isRight should ===(true)
    actual.right.foreach(_.size should ===(2))
    actual.right.foreach(a => a.head.copy(actions = a.head.actions.toVector) should ===(expected.head))
    actual.right.foreach(a => a.apply(1).copy(actions = a(1).actions.toVector) should ===(expected(1)))
  }
}
