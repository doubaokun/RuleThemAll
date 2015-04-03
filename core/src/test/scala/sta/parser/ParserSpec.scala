package sta.parser

import java.io.InputStream

import org.scalatest.{ Matchers, WordSpec }
import spire.implicits._
import spire.math.UByte
import sta.model.Definition
import sta.model.actions._
import sta.model.system._
import sta.model.triggers._
import sta.model.triggers.functions._
import kj.android.common.UsedFeatures._
import sta.tests.PropertyChecks

import scalaz.{ Equal, Tag }

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
          implicit def eqBP: Equal[BatteryPlugged] = Equal.equalA
          implicit def eqH: Equal[Headset] = Equal.equalA
          implicit def bt(l: BatteryLevel): UByte = l.value

          val expected = Vector(
            Definition(Tag("simple1"),
              AndTrigger(
                AndTrigger(
                  AtomicTrigger[BatteryPlugged](EqualFunction(BatteryPlugged.AC)),
                  AtomicTrigger[BatteryLevel](GTFunction(ub"50"))
                ),
                AtomicTrigger[Headset](EqualFunction(Headset.Connected))
              ),
              Vector(
                ChangeSoundProfile(Tag(1))
              )
            ),
            Definition(Tag("simple2"),
              XorTrigger(
                OrTrigger(
                  AtomicTrigger[BatteryPlugged](EqualFunction(BatteryPlugged.AC)),
                  AtomicTrigger[BatteryLevel](GTFunction(ub"50"))
                ),
                AtomicTrigger[Headset](EqualFunction(Headset.Connected))
              ),
              Vector(
                ChangeSoundProfile(Tag(1))
              )
            )
          )

          val actual = DSLParser.parse(getClass.getResourceAsStream("/simple"))

          "yield success" in {
            actual.isRight should ===(true)
          }

          "consists of two definitions" in {
            actual.foreach(_.size should ===(2))
          }

          "yield first definition equal to expected" in {
            actual.foreach(_.apply(0) should ===(expected(0)))
          }

          "yield second definition equal to expected" in {
            actual.foreach(_.apply(1) should ===(expected(1)))
          }
        }
      }
    }
  }
}
