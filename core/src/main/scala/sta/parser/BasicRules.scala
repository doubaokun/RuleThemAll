package sta.parser

import scala.language.{higherKinds, implicitConversions}
import fastparse.all._
import scala.annotation.tailrec
import scala.util.Try
import spire.math.{Natural => Nat, Rational, SafeLong, UByte, UInt}

trait BasicRules {
  private def digit = P(CharIn('0' to '9').!)

  private def digit19 = P(CharIn('1' to '9').!)

  private def hexDigit = P(CharIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')))

  private implicit def parserExtras[T](parser: Parser[T]): BasicRules.ParserExtras[T] =
    new BasicRules.ParserExtras[T](parser)

  sealed abstract class Converter[T](radix: Int, conv: (String, Int) => T) {
    def unapply(s: String): Option[T] = Try(conv(s, radix)).toOption
  }

  private object ToInt extends Converter[Int](10, java.lang.Integer.parseInt)

  private object ToHexInt extends Converter[Int](16, java.lang.Integer.parseInt)

  private object ToLong extends Converter[Long](10, java.lang.Long.parseLong)

  private object ToHexLong extends Converter[Long](16, java.lang.Long.parseLong)

  private object ToDouble {
    def unapply(s: String): Option[Double] = Try(java.lang.Double.parseDouble(s)).toOption
  }

  lazy val Percent: P[UByte] = P(
    ("0" | "100" | (digit19 ~ digit.?)).! ~ "%" map ((s: String) => UByte(s.toByte))
  )

  lazy val Byte: P[UByte] = {
    val p16: P[UByte] = for {
      ToHexInt(v) <- "0x" ~ hexDigit.rep(1).! if v <= 255
    } yield UByte(v)

    val p10: P[UByte] = for {
      ToInt(v) <- (("0" ~ !"x") | (digit19 ~ digit.rep)).! if v <= 255
    } yield UByte(v)

    P(p16 | p10)
  }

  lazy val UnsignedInt: P[UInt] = {
    val p16: P[UInt] = for {
      ToHexLong(v) <- "0x" ~ hexDigit.rep(1).! if v <= 4294967295L
    } yield UInt(v)

    val p10: P[UInt] = for {
      ToLong(v) <- (("0" ~ !"x") | (digit19 ~ digit.rep)).! if v <= 4294967295L
    } yield UInt(v)

    P(p16 | p10)
  }

  lazy val Int: P[Int] = {
    val p = for {
      ToInt(v) <- ("-".? ~ ("0" | (digit19 ~ digit.rep))).!
    } yield v

    P(p)
  }

  lazy val Natural: P[Nat] = P(
    P("0" | (digit19 ~ digit.rep)).! map (Nat(_))
  )

  lazy val Integer: P[SafeLong] = P(
    ("-".? ~ ("0" | (digit19 ~ digit.rep))).! map (SafeLong(_))
  )

  lazy val Float: P[Float] = {
    val p = for {
      ToDouble(v) <- ("-".? ~ digit.rep(1) ~ ("." ~ digit.rep(1)).?).!
      if v >= scala.Float.MinValue && v <= scala.Float.MaxValue
    } yield v.toFloat

    P(p)
  }

  lazy val Decimal: P[Rational] = P(
    ("-".? ~ ((digit.rep(1) ~ "/" ~ (digit19 ~ digit.rep)) |
      (digit.rep(1) ~ ("." ~ digit.rep(1) ~
        ("E" ~ "-".? ~ digit.rep(1)).?).?))).! map (Rational(_))
  )

  lazy val SingleLineString: P[String] = {
    val Q = "\""
    val NL = "\n"
    def singleChar = P("\\\"".! | "\\\\".! | (!(NL | Q) ~ AnyChar).!)

    P(Q ~ singleChar.rep.! ~ Q)
  }

  lazy val MultiLineString: P[String] = {
    val Q = "\""
    val TQ = "\"\"\""
    def tripleChar = P((Q.? ~ Q.? ~ !Q ~ AnyChar).!)

    P(TQ ~ tripleChar.rep.! ~ TQ)
  }

  lazy val String: P[String] = P(MultiLineString | SingleLineString)

  lazy val MacAddress: P[String] =
    P((hexDigit ~ hexDigit ~ ":" ~ hexDigit ~ hexDigit ~ ":" ~ hexDigit ~ hexDigit ~ ":" ~
      hexDigit ~ hexDigit ~ ":" ~ hexDigit ~ hexDigit ~ ":" ~ hexDigit ~ hexDigit ~ !(hexDigit | ":")).!)

  def mapParser[T](map: Map[String, T]): P[T] = {
    def makeRule(kv: (String, T)): P[T] = kv._1.! map (_ => kv._2)

    if (map.isEmpty) Fail
    else map.tail.foldLeft(makeRule(map.head)) {
      case (r, kv) =>
        r | makeRule(kv)
    }
  }
}

object BasicRules {

  class ParserExtras[T](private val parser: Parser[T]) extends AnyVal {
    def withFilter(p: T => Boolean) = parser.filter(p)
  }

}
