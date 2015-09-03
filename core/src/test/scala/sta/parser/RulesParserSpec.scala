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

  cronExpressionRule()

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
          ChangeSoundProfile(ChangeSoundProfile.Mode.Vibrate)
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
