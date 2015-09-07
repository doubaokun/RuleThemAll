package sta.parser

import scala.language.{higherKinds, implicitConversions}
import fastparse.all._
import kj.android.cron.{CronExpression => Cron}
import scala.concurrent.duration.{Duration => ScalaDuration, _}
import scala.util.Try
import spire.math.{Natural => Nat, Rational, SafeLong, UByte, UInt}
import sta.parser.WhitespaceSkip._

trait BasicRules extends WhitespaceSkip {
  private def digit = P(CharIn('0' to '9').!)

  private def digit19 = P(CharIn('1' to '9').!)

  private def hexDigit = P(CharIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')))

  private def variations(str: String): P[Unit] = s"${str}s" | str | s"${str.head}"

  implicit def parserExtras[T](parser: Parser[T]): BasicRules.ParserExtras[T] =
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

  implicit def liftToParser(str: String): BasicRules.LiftToParser = new BasicRules.LiftToParser(str)

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
    def makeRule(kv: (String, T)): P[T] = kv._1.lWS map (_ => kv._2)

    if (map.isEmpty) Fail
    else map.tail.foldLeft(makeRule(map.head)) {
      case (r, kv) =>
        r | makeRule(kv)
    }
  }

  lazy val CronExpression: P[Cron] = {
    def Value(min: Int, max: Int, abbreviations: Map[String, Int] = Map.empty) = {
      val ip = for {
        ToInt(v) <- digit.rep(1).! if v >= min && v <= max
      } yield v
      val p = if (abbreviations.nonEmpty) {
        val ap = abbreviations.keys.tail.foldLeft(abbreviations.keys.head.!) { _ | _.! }
        ip | ap.map(abbreviations)
      } else ip
      ("*" ~ "/" ~ ip).map(v => Cron.Range(min = min, max = max, step = v)) |
      "*".!.map(_ => Cron.Range(min = min, max = max)) |
        (p ~ "-" ~ p ~ "/" ~ ip).map(v => Cron.Range(min = v._1, max = v._2, step = v._3)) |
        (p ~ "-" ~ p).map(v => Cron.Range(min = v._1, max = v._2)) |
        p.rep(1, sep = ",").map(v => Cron.List(v.head, v.tail.sorted.distinct.toArray))
    }

    val months = Seq("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL",
      "AUG", "SEP", "OCT", "NOV", "DEC").zipWithIndex.toMap.mapValues(_ + 1)
    val daysOfWeak = Seq("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT").zipWithIndex.toMap
    P("\"" ~ Value(0, 59) ~ " " ~ Value(0, 23) ~ " " ~ Value(1, 31) ~ " " ~
      Value(1, 12, months) ~ " " ~ Value(0, 6, daysOfWeak) ~ (" " ~ Value(1970, 2099)).? ~ "\"" map {
      case (minute, hour, dayOfMonth, month, dayOfWeek, year) => Cron(
        minute = minute,
        hour = hour,
        dayOfMonth = dayOfMonth,
        month = month,
        dayOfWeek = dayOfWeek,
        year = year
      )
    })
  }

  lazy val Duration: P[ScalaDuration] = P(
    (UnsignedInt.filter(_.toInt <= 59) ~ NoCut(WL) ~ variations("second") map (_.toInt.seconds)) |
      (UnsignedInt.filter(_.toInt <= 59) ~ NoCut(WL) ~ variations("minute") map (_.toInt.minutes)) |
      (UnsignedInt.filter(_.toInt <= 23) ~ NoCut(WL) ~ variations("hour") map (_.toInt.hours))
  )
}

object BasicRules extends WhitespaceSkip {
  implicit class LiftToParser(val str: String) extends AnyVal {
    def lWS: Parser[Unit] = {
      val splitted = str.split("\\s+")
      splitted.tail.foldLeft(splitted.head: P[Unit]) { _ ~ NoCut(WL) ~ _ }
    }

    def l: Parser[Unit] = wspStr(str)
  }

  class ParserExtras[T](private val parser: Parser[T]) extends AnyVal {
    def withFilter(p: T => Boolean) = parser.filter(p)
  }
}
