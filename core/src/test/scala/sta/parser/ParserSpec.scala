package sta.parser

import scala.language.implicitConversions

import android.media.AudioManager
import java.io.InputStream
import kj.android.common.UsedFeatures._
import org.scalatest.{Matchers, WordSpec}
import spire.implicits._
import sta.model.Rule
import sta.model.actions._
import sta.model.system._
import sta.model.triggers._
import sta.tests.PropertyChecks

class ParserSpec extends WordSpec with PropertyChecks with Matchers with ParserHelpers with BasicRulesSpec {
  implicit def loadInputStream(is: InputStream): String = io.Source.fromInputStream(is).mkString

  "A Parser" when {
    "dealing with input" which {
      "is literal" when {
        usingPercentRule()
        usingByteRule()
        usingUnsignedIntRule()
        usingIntRule()
        usingNaturalRule()
        usingIntegerRule()

        usingFloatRule()
        usingDecimalRule()

        usingStringRule()
      }

      "is script" when {
        "parsing simple" should {
          val expected = Vector(
            Rule(
              "simple1",
              AndTrigger(
                AndTrigger(
                  AtomicTrigger[Battery](_.plugged == Battery.Plugged.withName("ac")),
                  AtomicTrigger[Battery](_.level > ub"50")
                ),
                AtomicTrigger[Headset](_ == Headset.withName("connected"))
              ),
              Vector(
                ChangeSoundProfile(ChangeSoundProfile.Mode.Vibrate)
              )
            ),
            Rule(
              "simple2",
              XorTrigger(
                OrTrigger(
                  AtomicTrigger[Battery](_.plugged == Battery.Plugged.withName("usb")),
                  AtomicTrigger[Battery](_.level <= ub"70")
                ),
                AtomicTrigger[Headset](_ == Headset.withName("disconnected"))
              ),
              Vector(
                ChangeSoundProfile(ChangeSoundProfile.Mode.Silent)
              )
            )
          )

          val actual = RulesParser.parse(getClass.getResourceAsStream("/simple"))

          "yield success" in {
            actual.isRight should ===(true)
          }

          "consists of two definitions" in {
            actual.right.foreach(_.size should ===(2))
          }

          "yield first definition equal to expected" in {
            actual.right.foreach(a => a.head.copy(actions = a.head.actions.toVector) should ===(expected.head))
          }

          "yield second definition equal to expected" in {
            actual.right.foreach(a => a.apply(1).copy(actions = a(1).actions.toVector) should ===(expected(1)))
          }
        }
      }
    }
  }
}
