package sta.parser

import scala.language.implicitConversions
import java.io.InputStream
import org.scalatest.{FlatSpec, Matchers}
import spire.implicits._
import sta.common.Uses._
import sta.model.Rule
import sta.model.actions._
import sta.model.triggers.Implicits._
import sta.model.triggers.Trigger.Branch
import sta.model.triggers._
import sta.tests.PropertyChecks

class RulesParserSpec extends FlatSpec with PropertyChecks with Matchers with ParserHelpers with BasicRulesSpec {
  implicit def loadInputStream(is: InputStream): String = io.Source.fromInputStream(is).mkString

  percentRule()

  byteRule()

  unsignedIntRule()

  intRule()

  naturalRule()

  integerRule()

  floatRule()

  decimalRule()

  stringRule()

  macAddressRule()

  behavior of "RulesParser"

  it should "parse whole scripts" in {
    val expected = Vector(
      Rule(
        "simple1",
        Seq(
          Branch(List(
            Trigger.Atomic[Battery](_.plugged == Battery.Plugged.withName("ac")),
            Trigger.Atomic[Battery](_.level > ub"50"),
            Trigger.Atomic[Headset](_ == Headset.withName("connected"))
          ))
        ),
        Vector(
          ChangeSoundProfile(ChangeSoundProfile.Mode.Vibrate)
        )
      ),
      Rule(
        "simple2",
        Seq(
          Branch(List(
            Trigger.Atomic[Battery](_.plugged == Battery.Plugged.withName("usb")),
            Trigger.Atomic[Battery](_.level <= ub"70"),
            Trigger.Atomic[Headset](h => !(h == Headset.withName("disconnected")))
          )),
          Branch(List(
            Trigger.Atomic[Battery](b => !(b.plugged == Battery.Plugged.withName("usb"))),
            Trigger.Atomic[Battery](b => !(b.level <= ub"70")),
            Trigger.Atomic[Headset](_ == Headset.withName("disconnected"))
          ))
        ),
        Vector(
          ChangeSoundProfile(ChangeSoundProfile.Mode.Silent)
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
