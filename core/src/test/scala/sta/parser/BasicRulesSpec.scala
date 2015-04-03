package sta.parser

import org.parboiled2.Parser.DeliveryScheme.Throw
import org.parboiled2._
import org.scalacheck.Gen
import org.scalatest.{ Matchers, WordSpec }
import spire.math._
import sta.tests.PropertyChecks

trait BasicRulesSpec { this: WordSpec with PropertyChecks with Matchers with ParserHelpers ⇒
  private class TestParser(input: ParserInput) extends DSLParser(input)

  def usingByteRule(): Unit = {
    "using byte parser rule" should {
      "yield unsigned byte on in range literal" in {
        forAll(Gen.chooseNum(Byte.MinValue, Byte.MaxValue)) { _num ⇒
          val num = UByte(_num)
          val parser = new TestParser(num.toString)
          parser.Byte.run() should ===(num)
        }
      }

      "throw error on negative literal" in {
        forAll(Gen.chooseNum(Byte.MinValue, -1)) { num ⇒
          val parser = new TestParser(num.toString)
          intercept[ParseError] {
            parser.Byte.run()
          }
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.chooseNum(256, Int.MaxValue)) { num ⇒
          val parser = new TestParser(num.toString)
          intercept[ParseError] {
            parser.Byte.run()
          }
        }
      }

      "yield unsigned byte on in range hex literal" in {
        forAll(Gen.chooseNum(1, 255)) { _num ⇒
          val hex = "0x" + java.lang.Integer.toHexString(_num)
          val num = UByte(_num)
          val parser = new TestParser(hex)
          parser.Byte.run() should ===(num)
        }
      }

      "throw error on out of range hex literal" in {
        forAll(Gen.chooseNum(256, Int.MaxValue)) { _num ⇒
          val hex = "0x" + java.lang.Integer.toHexString(_num)
          val parser = new TestParser(hex)
          intercept[ParseError] {
            parser.Byte.run()
          }
        }
      }
    }
  }

  def usingUnsignedIntRule(): Unit = {
    "using unsigned int parser rule" should {
      "yield unsigned int on in range literal" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { _num ⇒
          val num = UInt(_num)
          val parser = new TestParser(num.toString)
          parser.UnsignedInt.run() should ===(num)
        }
      }

      "throw error on negative literal" in {
        forAll(Gen.chooseNum(Int.MinValue, -1)) { num ⇒
          val parser = new TestParser(num.toString)
          intercept[ParseError] {
            parser.UnsignedInt.run()
          }
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.chooseNum(UInt.MaxValue.toLong + 1, Long.MaxValue)) { num ⇒
          val parser = new TestParser(num.toString)
          intercept[ParseError] {
            parser.UnsignedInt.run()
          }
        }
      }

      "yield unsigned int on in range hex literal" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { _num ⇒
          val hex = "0x" + java.lang.Integer.toHexString(_num)
          val num = UInt(_num)
          val parser = new TestParser(hex)
          parser.UnsignedInt.run() should ===(num)
        }
      }

      "throw error on out of range hex literal" in {
        forAll(Gen.chooseNum(UInt.MaxValue.toLong + 1, Long.MaxValue)) { _num ⇒
          val hex = "0x" + java.lang.Long.toHexString(_num)
          val parser = new TestParser(hex)
          intercept[ParseError] {
            parser.UnsignedInt.run()
          }
        }
      }
    }
  }

  def usingIntRule(): Unit = {
    "using int parser rule" should {
      "yield int on in range literal" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { num ⇒
          val parser = new TestParser(num.toString)
          parser.Int.run() should ===(num)
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.oneOf(
          Gen.chooseNum(Long.MinValue, Int.MinValue.toLong - 1),
          Gen.chooseNum(Int.MaxValue.toLong + 1, Long.MaxValue)
        )) { num ⇒
          val parser = new TestParser(num.toString)
          intercept[ParseError] {
            parser.Int.run()
          }
        }
      }
    }
  }

  def usingPercentRule(): Unit = {
    "using percent parser rule" should {
      "yield unsigned byte on in range literal" in {
        forAll(Gen.chooseNum[Byte](0, 100)) { _num ⇒
          val num = UByte(_num)
          val parser = new TestParser(num.toString + '%')
          parser.Percent.run() should ===(num)
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue).suchThat(num ⇒ num < 0 || num > 100)) { num ⇒
          val parser = new TestParser(num.toString + '%')
          intercept[ParseError] {
            parser.Percent.run()
          }
        }
      }

      "throw error on literal without '%'" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue, (0 to 100).toSeq: _*)) { num ⇒
          val parser = new TestParser(num.toString)
          intercept[ParseError] {
            parser.Percent.run()
          }
        }
      }
    }
  }

  def usingNaturalRule(): Unit = {
    "using natural number parser rule" should {
      "yield natural on in range literal" in {
        forAll(Gen.nonEmptyNumStr) { _num ⇒
          val num = Natural(_num)
          val parser = new TestParser(num.toString)
          parser.Natural.run() should ===(num)
        }
      }

      "throw error on negative literal" in {
        forAll(Gen.nonEmptyNumStr.suchThat(_ != "0").map('-' + _)) { num ⇒
          val parser = new TestParser(num)
          intercept[ParseError] {
            parser.Natural.run()
          }
        }
      }
    }
  }

  def usingIntegerRule(): Unit = {
    "using integer parser rule" should {
      "yield integer" in {
        forAll(Gen.oneOf(Gen.nonEmptyNumStr, Gen.nonEmptyNumStr.suchThat(_ != "0").map('-' + _))) { _num ⇒
          val num = SafeLong(_num)
          val parser = new TestParser(num.toString)
          parser.Integer.run() should ===(num)
        }
      }
    }
  }

  def usingFloatRule(): Unit = {
    "using float parser rule" should {
      "yield float on in range literal" in {
        forAll(Gen.floatString(v ⇒ Float.MinValue <= v && v <= Float.MaxValue)) { num ⇒
          val parser = new TestParser(num)
          parser.Float.run() should ===(num.toFloat)
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.floatString(v ⇒ Float.MinValue > v || v > Float.MaxValue)) { num ⇒
          val parser = new TestParser(num.toString)
          intercept[ParseError] {
            parser.Float.run()
          }
        }
      }
    }
  }

  def usingDecimalRule(): Unit = {
    "using decimal parser rule" should {
      "yield decimal" in {
        forAll(Gen.oneOf(Gen.chooseNum(Double.MinValue, Double.MaxValue).map(_.toString), Gen.rationalString)) { _num ⇒
          val parser = new TestParser(_num)
          parser.Decimal.run() should ===(Rational(_num))
        }
      }
    }
  }

  def usingStringRule(): Unit = {
    "using string parser rule" should {
      "yield string for single-line string" in {
        forAll(Gen.alphaStr) { s ⇒
          val parser = new TestParser("\"" + s + "\"")
          parser.String.run() should ===(s)
        }
      }

      "yield string for multi-line string with '\\n'" in {
        forAll(Gen.nonEmptyListOf(Gen.alphaStr).map(_.mkString("\n"))) { s ⇒
          val parser = new TestParser("\"\"\"" + s + "\"\"\"")
          parser.String.run() should ===(s)
        }
      }

      "yield string for multi-line string with '\\r\\n'" in {
        forAll(Gen.nonEmptyListOf(Gen.alphaStr).map(_.mkString("\r\n"))) { s ⇒
          val parser = new TestParser("\"\"\"" + s + "\"\"\"")
          parser.String.run() should ===(s)
        }
      }
    }
  }
}
