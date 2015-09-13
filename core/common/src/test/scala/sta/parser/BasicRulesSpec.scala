package sta.parser

import fastparse.all._
import fastparse.core.SyntaxError
import kj.android.cron.CronExpression
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._
import spire.math._
import sta.tests.PropertyChecks

class BasicRulesSpec extends FlatSpec with PropertyChecks with Matchers with ParserHelpers {

  private object BasicParser extends BasicRules {
    def apply[T](rule: BasicRules => P[T]): P[T] = P(Start ~ rule(this) ~ End)
  }

  behavior of "Byte rule"

  it should "yield unsigned byte on in range literal" in {
    forAll(Gen.chooseNum(Byte.MinValue, Byte.MaxValue)) { _num =>
      val num = UByte(_num)
      BasicParser(_.Byte).parse(num.toString()).get.value should ===(num)
    }
  }

  it should "throw error on negative literal" in {
    forAll(Gen.chooseNum(Byte.MinValue, -1)) { num =>
      intercept[SyntaxError] {
        BasicParser(_.Byte).parse(num.toString).get
      }
    }
  }

  it should "throw error on out of range literal" in {
    forAll(Gen.chooseNum(256, Int.MaxValue)) { num =>
      intercept[SyntaxError] {
        BasicParser(_.Byte).parse(num.toString).get
      }
    }
  }

  it should "yield unsigned byte on in range hex literal" in {
    forAll(Gen.chooseNum(1, 255)) { _num =>
      val hex = "0x" + java.lang.Integer.toHexString(_num)
      val num = UByte(_num)
      BasicParser(_.Byte).parse(hex).get.value should ===(num)
    }
  }

  it should "throw error on out of range hex literal" in {
    forAll(Gen.chooseNum(256, Int.MaxValue)) { _num =>
      val hex = "0x" + java.lang.Integer.toHexString(_num)
      intercept[SyntaxError] {
        BasicParser(_.Byte).parse(hex).get
      }
    }
  }

  behavior of "UnsignedInt rule"

  it should "yield unsigned int on in range literal" in {
    forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { _num =>
      val num = UInt(_num)
      BasicParser(_.UnsignedInt).parse(num.toString()).get.value should ===(num)
    }
  }

  it should "throw error on negative literal" in {
    forAll(Gen.chooseNum(Int.MinValue, -1)) { num =>
      intercept[SyntaxError] {
        BasicParser(_.UnsignedInt).parse(num.toString).get
      }
    }
  }

  it should "throw error on out of range literal" in {
    forAll(Gen.chooseNum(UInt.MaxValue.toLong + 1, Long.MaxValue)) { num =>
      intercept[SyntaxError] {
        BasicParser(_.UnsignedInt).parse(num.toString).get
      }
    }
  }

  it should "yield unsigned int on in range hex literal" in {
    forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { _num =>
      val hex = "0x" + java.lang.Integer.toHexString(_num)
      val num = UInt(_num)
      BasicParser(_.UnsignedInt).parse(hex).get.value should ===(num)
    }
  }

  it should "throw error on out of range hex literal" in {
    forAll(Gen.chooseNum(UInt.MaxValue.toLong + 1, Long.MaxValue)) { _num =>
      val hex = "0x" + java.lang.Long.toHexString(_num)
      intercept[SyntaxError] {
        BasicParser(_.UnsignedInt).parse(hex).get
      }
    }
  }

  behavior of "Int rule"

  it should "yield int on in range literal" in {
    forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { num =>
      BasicParser(_.Int).parse(num.toString).get.value should ===(num)
    }
  }

  it should "throw error on out of range literal" in {
    forAll(Gen.oneOf(
      Gen.chooseNum(Long.MinValue, Int.MinValue.toLong - 1),
      Gen.chooseNum(Int.MaxValue.toLong + 1, Long.MaxValue)
    )) { num =>
      intercept[SyntaxError] {
        BasicParser(_.Int).parse(num.toString).get
      }
    }
  }

  behavior of "Percent rule"

  it should "yield unsigned byte on in range literal" in {
    forAll(Gen.chooseNum[Byte](0, 100)) { _num =>
      val num = UByte(_num)
      BasicParser(_.Percent).parse(num.toString + '%').get.value should ===(num)
    }
  }

  it should "throw error on out of range literal" in {
    forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue).suchThat(num =>
      num < 0 || num > 100)) { num =>
      intercept[SyntaxError] {
        BasicParser(_.Percent).parse(num.toString + '%').get
      }
    }
  }

  it should "throw error on literal without '%'" in {
    forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue, (0 to 100).toSeq: _*)) { num =>
      intercept[SyntaxError] {
        BasicParser(_.Percent).parse(num.toString).get
      }
    }
  }

  behavior of "Natural rule"

  it should "yield natural on in range literal" in {
    forAll(Gen.nonEmptyNumStr) { _num =>
      val num = Natural(_num)
      BasicParser(_.Natural).parse(num.toString()).get.value should ===(num)
    }
  }

  it should "throw error on negative literal" in {
    forAll(Gen.nonEmptyNumStr.suchThat(_ != "0").map('-' + _)) { num =>
      intercept[SyntaxError] {
        BasicParser(_.Natural).parse(num).get
      }
    }
  }

  behavior of "Integer rule"

  it should "yield integer" in {
    forAll(Gen.oneOf(
      Gen.nonEmptyNumStr,
      Gen.nonEmptyNumStr.suchThat(_ != "0").map('-' + _)
    )) { _num =>
      val num = SafeLong(BigInt(_num))
      BasicParser(_.Integer).parse(num.toString()).get.value should ===(num)
    }
  }

  behavior of "Float rule"

  it should "yield float on in range literal" in {
    forAll(Gen.floatString(v => Float.MinValue <= v && v <= Float.MaxValue)) { num =>
      BasicParser(_.Float).parse(num).get.value should ===(num.toFloat)
    }
  }

  it should "throw error on out of range literal" in {
    forAll(Gen.floatString(v => Float.MinValue > v || v > Float.MaxValue)) { num =>
      intercept[SyntaxError] {
        BasicParser(_.Float).parse(num.toString).get
      }
    }
  }

  behavior of "Decimal rule"

  it should "yield decimal" in {
    forAll(Gen.oneOf(
      Gen.chooseNum(Double.MinValue, Double.MaxValue).map(_.toString),
      Gen.rationalString
    )) { _num =>
      BasicParser(_.Decimal).parse(_num).get.value should ===(Rational(_num))
    }
  }

  behavior of "String rule"

  it should "yield string for single-line string" in {
    forAll(Gen.alphaStr) { s =>
      BasicParser(_.String).parse("\"" + s + "\"").get.value should ===(s)
    }
  }

  it should "yield string for multi-line string with '\\n'" in {
    forAll(Gen.nonEmptyListOf(Gen.alphaStr).map(_.mkString("\n"))) { s =>
      BasicParser(_.String).parse("\"\"\"" + s + "\"\"\"").get.value should ===(s)
    }
  }

  it should "yield string for multi-line string with '\\r\\n'" in {
    forAll(Gen.nonEmptyListOf(Gen.alphaStr).map(_.mkString("\r\n"))) { s =>
      BasicParser(_.String).parse("\"\"\"" + s + "\"\"\"").get.value should ===(s)
    }
  }

  behavior of "MacAddress rule"

  it should "yield proper mac address string" in {
    forAll(Gen.hexString(12)) { s =>
      val mac = s.grouped(2).mkString(":")
      BasicParser(_.MacAddress).parse(mac).get.value should ===(mac)
    }
  }

  it should "throw error on to small or to long literal" in {
    forAll(Gen.hexString.suchThat(_.length != 12)) { s =>
      val mac = s.grouped(2).mkString(":")
      intercept[SyntaxError] {
        BasicParser(_.MacAddress).parse(mac).get.value
      }
    }
  }

  it should "throw error on out of a range literal" in {
    forAll(Gen.alphaStr.suchThat(_.exists(_.toLower > 'f'))) { s =>
      val mac = s.grouped(2).mkString(":")
      intercept[SyntaxError] {
        BasicParser(_.MacAddress).parse(mac).get.value
      }
    }
  }

  behavior of "CronExpression rule"

  it should "yield proper cron expression" in {
    val expr1 = """"* * * * * *""""
    val expected1 = CronExpression(
      minute = CronExpression.Range(0, 59),
      hour = CronExpression.Range(0, 23),
      dayOfMonth = CronExpression.Range(1, 31),
      month = CronExpression.Range(1, 12),
      dayOfWeek = CronExpression.Range(0, 6),
      year = Some(CronExpression.Range(1970, 2099))
    )
    val actual1 = BasicParser(_.CronExpression).parse(expr1).get.value
    actual1 should ===(expected1)

    val expr2 = """"*/5 0-12/2 1,11,21,31 JAN-JUN/2 TUE-4""""
    val el = CronExpression.List(1, Array(11, 21, 31))
    val expected2 = CronExpression(
      minute = CronExpression.Range(0, 59, 5),
      hour = CronExpression.Range(0, 12, 2),
      dayOfMonth = el,
      month = CronExpression.Range(1, 6, 2),
      dayOfWeek = CronExpression.Range(2, 4),
      year = None
    )
    val actual2 = BasicParser(_.CronExpression).parse(expr2).get.value
    actual2.copy(dayOfMonth = CronExpression.Range(1, 31)) should
      ===(expected2.copy(dayOfMonth = CronExpression.Range(1, 31)))
    actual2.dayOfMonth shouldBe a[CronExpression.List]
    val al = actual2.dayOfMonth.asInstanceOf[CronExpression.List]
    al.min should ===(el.min)
    al.rest should ===(el.rest)
  }


  behavior of "Duration rule"

  it should "parse seconds" in {
    forAll(Gen.chooseNum(0, 59), Gen.oneOf("s", "second", "seconds")) { (seconds, suffix) =>
      BasicParser(_.Duration).parse(s"$seconds $suffix").get.value should ===(seconds.seconds)
    }
  }

  it should "parse minutes" in {
    forAll(Gen.chooseNum(0, 59), Gen.oneOf("m", "minute", "minutes")) { (minutes, suffix) =>
      BasicParser(_.Duration).parse(s"$minutes $suffix").get.value should ===(minutes.minutes)
    }
  }

  it should "parse hours" in {
    forAll(Gen.chooseNum(0, 23), Gen.oneOf("h", "hour", "hours")) { (hours, suffix) =>
      BasicParser(_.Duration).parse(s"$hours $suffix").get.value should ===(hours.hours)
    }
  }

  it should "throw error on out of range literal" in {
    forAll(Gen.chooseNum(60, Int.MaxValue), Gen.oneOf("s", "second", "seconds")) { (seconds, suffix) =>
      intercept[SyntaxError] {
        BasicParser(_.Duration).parse(s"$seconds $suffix").get
      }
    }

    forAll(Gen.chooseNum(60, Int.MaxValue), Gen.oneOf("m", "minute", "minutes")) { (minutes, suffix) =>
      intercept[SyntaxError] {
        BasicParser(_.Duration).parse(s"$minutes $suffix").get
      }
    }

    forAll(Gen.chooseNum(24, Int.MaxValue), Gen.oneOf("h", "hour", "hours")) { (hours, suffix) =>
      intercept[SyntaxError] {
        BasicParser(_.Duration).parse(s"$hours $suffix").get
      }
    }
  }
}