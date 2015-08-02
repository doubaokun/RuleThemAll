package sta.parser

import fastparse.core.SyntaxError
import org.scalacheck.Gen
import org.scalatest.{ Matchers, WordSpec }
import spire.math._
import sta.tests.PropertyChecks

trait BasicRulesSpec {
  this: WordSpec with PropertyChecks with Matchers with ParserHelpers =>

  private object BasicParser extends BasicRules

  def usingByteRule(): Unit = {
    "using byte parser rule" should {
      "yield unsigned byte on in range literal" in {
        forAll(Gen.chooseNum(Byte.MinValue, Byte.MaxValue)) { _num =>
          val num = UByte(_num)
          BasicParser.Byte.parse(num.toString()).get.value should ===(num)
        }
      }

      "throw error on negative literal" in {
        forAll(Gen.chooseNum(Byte.MinValue, -1)) { num =>
          intercept[SyntaxError] {
            BasicParser.Byte.parse(num.toString).get
          }
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.chooseNum(256, Int.MaxValue)) { num =>
          intercept[SyntaxError] {
            BasicParser.Byte.parse(num.toString).get
          }
        }
      }

      "yield unsigned byte on in range hex literal" in {
        forAll(Gen.chooseNum(1, 255)) { _num =>
          val hex = "0x" + java.lang.Integer.toHexString(_num)
          val num = UByte(_num)
          BasicParser.Byte.parse(hex).get.value should ===(num)
        }
      }

      "throw error on out of range hex literal" in {
        forAll(Gen.chooseNum(256, Int.MaxValue)) { _num =>
          val hex = "0x" + java.lang.Integer.toHexString(_num)
          intercept[SyntaxError] {
            BasicParser.Byte.parse(hex).get
          }
        }
      }
    }
  }

  def usingUnsignedIntRule(): Unit = {
    "using unsigned int parser rule" should {
      "yield unsigned int on in range literal" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { _num =>
          val num = UInt(_num)
          BasicParser.UnsignedInt.parse(num.toString()).get.value should ===(num)
        }
      }

      "throw error on negative literal" in {
        forAll(Gen.chooseNum(Int.MinValue, -1)) { num =>
          intercept[SyntaxError] {
            BasicParser.UnsignedInt.parse(num.toString).get
          }
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.chooseNum(UInt.MaxValue.toLong + 1, Long.MaxValue)) { num =>
          intercept[SyntaxError] {
            BasicParser.UnsignedInt.parse(num.toString).get
          }
        }
      }

      "yield unsigned int on in range hex literal" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { _num =>
          val hex = "0x" + java.lang.Integer.toHexString(_num)
          val num = UInt(_num)
          BasicParser.UnsignedInt.parse(hex).get.value should ===(num)
        }
      }

      "throw error on out of range hex literal" in {
        forAll(Gen.chooseNum(UInt.MaxValue.toLong + 1, Long.MaxValue)) { _num =>
          val hex = "0x" + java.lang.Long.toHexString(_num)
          intercept[SyntaxError] {
            BasicParser.UnsignedInt.parse(hex).get
          }
        }
      }
    }
  }

  def usingIntRule(): Unit = {
    "using int parser rule" should {
      "yield int on in range literal" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { num =>
          BasicParser.Int.parse(num.toString).get.value should ===(num)
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.oneOf(
          Gen.chooseNum(Long.MinValue, Int.MinValue.toLong - 1),
          Gen.chooseNum(Int.MaxValue.toLong + 1, Long.MaxValue)
        )) { num =>
          intercept[SyntaxError] {
            BasicParser.Int.parse(num.toString).get
          }
        }
      }
    }
  }

  def usingPercentRule(): Unit = {
    "using percent parser rule" should {
      "yield unsigned byte on in range literal" in {
        forAll(Gen.chooseNum[Byte](0, 100)) { _num =>
          val num = UByte(_num)
          BasicParser.Percent.parse(num.toString + '%').get.value should ===(num)
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue).suchThat(num =>
          num < 0 || num > 100)) { num =>
          intercept[SyntaxError] {
            BasicParser.Percent.parse(num.toString + '%').get
          }
        }
      }

      "throw error on literal without '%'" in {
        forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue, (0 to 100).toSeq: _*)) { num =>
          intercept[SyntaxError] {
            BasicParser.Percent.parse(num.toString).get
          }
        }
      }
    }
  }

  def usingNaturalRule(): Unit = {
    "using natural number parser rule" should {
      "yield natural on in range literal" in {
        forAll(Gen.nonEmptyNumStr) { _num =>
          val num = Natural(_num)
          BasicParser.Natural.parse(num.toString()).get.value should ===(num)
        }
      }

      "throw error on negative literal" in {
        forAll(Gen.nonEmptyNumStr.suchThat(_ != "0").map('-' + _)) { num =>
          intercept[SyntaxError] {
            BasicParser.Natural.parse(num).get
          }
        }
      }
    }
  }

  def usingIntegerRule(): Unit = {
    "using integer parser rule" should {
      "yield integer" in {
        forAll(Gen.oneOf(
          Gen.nonEmptyNumStr,
          Gen.nonEmptyNumStr.suchThat(_ != "0").map('-' + _)
        )) { _num =>
          val num = SafeLong(_num)
          BasicParser.Integer.parse(num.toString()).get.value should ===(num)
        }
      }
    }
  }

  def usingFloatRule(): Unit = {
    "using float parser rule" should {
      "yield float on in range literal" in {
        forAll(Gen.floatString(v => Float.MinValue <= v && v <= Float.MaxValue)) { num =>
          BasicParser.Float.parse(num).get.value should ===(num.toFloat)
        }
      }

      "throw error on out of range literal" in {
        forAll(Gen.floatString(v => Float.MinValue > v || v > Float.MaxValue)) { num =>
          intercept[SyntaxError] {
            BasicParser.Float.parse(num.toString).get
          }
        }
      }
    }
  }

  def usingDecimalRule(): Unit = {
    "using decimal parser rule" should {
      "yield decimal" in {
        forAll(Gen.oneOf(
          Gen.chooseNum(Double.MinValue, Double.MaxValue).map(_.toString),
          Gen.rationalString
        )) { _num =>
          BasicParser.Decimal.parse(_num).get.value should ===(Rational(_num))
        }
      }
    }
  }

  def usingStringRule(): Unit = {
    "using string parser rule" should {
      "yield string for single-line string" in {
        forAll(Gen.alphaStr) { s =>
          BasicParser.String.parse("\"" + s + "\"").get.value should ===(s)
        }
      }

      "yield string for multi-line string with '\\n'" in {
        forAll(Gen.nonEmptyListOf(Gen.alphaStr).map(_.mkString("\n"))) { s =>
          BasicParser.String.parse("\"\"\"" + s + "\"\"\"").get.value should ===(s)
        }
      }

      "yield string for multi-line string with '\\r\\n'" in {
        forAll(Gen.nonEmptyListOf(Gen.alphaStr).map(_.mkString("\r\n"))) { s =>
          BasicParser.String.parse("\"\"\"" + s + "\"\"\"").get.value should ===(s)
        }
      }
    }
  }
}
