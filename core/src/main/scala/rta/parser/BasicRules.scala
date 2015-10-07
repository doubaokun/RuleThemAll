package rta.parser

import fastparse.all._
import scala.concurrent.duration.{Duration => ScalaDuration, _}
import scala.util.Try
import spire.math.{Natural => Nat, Rational, SafeLong, UByte, UInt}
import rta.cron.CronExpression
import rta.parser.Extras._

trait BasicRules {
  private def digit = P(CharIn('0' to '9').!)

  private def digit19 = P(CharIn('1' to '9').!)

  private def hexDigit = P(CharIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')))

  private def variations(str: String): P[Unit] = s"${str}s" | str | s"${str.head}"

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

  def Percent: P[UByte] = P(
    ("0" | "100" | (digit19 ~ digit.?)).! ~ "%" map ((s: String) => UByte(s.toByte))
  )

  def Byte: P[UByte] = {
    val p16: P[UByte] = for {
      ToHexInt(v) <- "0x" ~ hexDigit.rep(1).! if v <= 255
    } yield UByte(v)

    val p10: P[UByte] = for {
      ToInt(v) <- (("0" ~ !"x") | (digit19 ~ digit.rep)).! if v <= 255
    } yield UByte(v)

    P(p16 | p10)
  }

  def UnsignedInt: P[UInt] = {
    val p16: P[UInt] = for {
      ToHexLong(v) <- "0x" ~ hexDigit.rep(1).! if v <= 4294967295L
    } yield UInt(v)

    val p10: P[UInt] = for {
      ToLong(v) <- (("0" ~ !"x") | (digit19 ~ digit.rep)).! if v <= 4294967295L
    } yield UInt(v)

    P(p16 | p10)
  }

  def Int: P[Int] = {
    val p = for {
      ToInt(v) <- ("-".? ~ ("0" | (digit19 ~ digit.rep))).!
    } yield v

    P(p)
  }

  def Natural: P[Nat] = P(
    P("0" | (digit19 ~ digit.rep)).! map (Nat(_))
  )

  def Integer: P[SafeLong] = P(
    ("-".? ~ ("0" | (digit19 ~ digit.rep))).! map (s => SafeLong(BigInt(s)))
  )

  def Float: P[Float] = {
    val p = for {
      ToDouble(v) <- ("-".? ~ digit.rep(1) ~ ("." ~ digit.rep(1)).?).!
      if v >= scala.Float.MinValue && v <= scala.Float.MaxValue
    } yield v.toFloat

    P(p)
  }

  def Decimal: P[Rational] = P(
    ("-".? ~ ((digit.rep(1) ~ "/" ~ (digit19 ~ digit.rep)) |
      (digit.rep(1) ~ ("." ~ digit.rep(1) ~
        ("E" ~ "-".? ~ digit.rep(1)).?).?))).! map (Rational(_))
  )

  def SingleLineString: P[String] = {
    val Q = "\""
    val NL = "\n"
    def singleChar = P("\\\"".! | "\\\\".! | (!(NL | Q) ~ AnyChar).!)

    P(Q ~ singleChar.rep.! ~ Q)
  }

  def MultiLineString: P[String] = {
    val Q = "\""
    val TQ = "\"\"\""
    def tripleChar = P((Q.? ~ Q.? ~ !Q ~ AnyChar).!)

    P(TQ ~ tripleChar.rep.! ~ TQ)
  }

  def String: P[String] = P(MultiLineString | SingleLineString)

  def MacAddress: P[String] =
    P((hexDigit ~ hexDigit ~ ":" ~ hexDigit ~ hexDigit ~ ":" ~ hexDigit ~ hexDigit ~ ":" ~
      hexDigit ~ hexDigit ~ ":" ~ hexDigit ~ hexDigit ~ ":" ~ hexDigit ~ hexDigit ~ !(hexDigit | ":")).!)

  def mapParser[T](map: Map[String, T]): P[T] = {
    def makeRule(kv: (String, T)): P[T] = kv._1.splitWS.map(_ => kv._2)

    if (map.isEmpty) Fail
    else map.tail.foldLeft(makeRule(map.head)) {
      case (r, kv) =>
        r | makeRule(kv)
    }
  }

  def CronExpr: P[CronExpression] = {
    def Value(min: Int, max: Int, abbreviations: Map[String, Int] = Map.empty) = {
      val ip = for {
        ToInt(v) <- digit.rep(1).! if v >= min && v <= max
      } yield v
      val p = if (abbreviations.nonEmpty) {
        val ap = abbreviations.keys.tail.foldLeft(abbreviations.keys.head.!) { _ | _.! }
        ip | ap.map(abbreviations)
      } else ip
      ("*" ~ "/" ~ ip).map(v => CronExpression.Range(min = min, max = max, step = v)) |
      "*".!.map(_ => CronExpression.Range(min = min, max = max)) |
        (p ~ "-" ~ p ~ "/" ~ ip).map(v => CronExpression.Range(min = v._1, max = v._2, step = v._3)) |
        (p ~ "-" ~ p).map(v => CronExpression.Range(min = v._1, max = v._2)) |
        p.rep(1, sep = ",").map(v => CronExpression.List(v.head, v.tail.sorted.distinct.toArray))
    }

    val months = Seq("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL",
      "AUG", "SEP", "OCT", "NOV", "DEC").zipWithIndex.toMap.mapValues(_ + 1)
    val daysOfWeak = Seq("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT").zipWithIndex.toMap
    P("\"" ~ Value(0, 59) ~ " " ~ Value(0, 23) ~ " " ~ Value(1, 31) ~ " " ~
      Value(1, 12, months) ~ " " ~ Value(0, 6, daysOfWeak) ~ (" " ~ Value(1970, 2099)).? ~ "\"" map {
      case (minute, hour, dayOfMonth, month, dayOfWeek, year) => CronExpression(
        minute = minute,
        hour = hour,
        dayOfMonth = dayOfMonth,
        month = month,
        dayOfWeek = dayOfWeek,
        year = year
      )
    })
  }

  def Duration: P[ScalaDuration] = P(
    (UnsignedInt.filter(_.toInt <= 59) ~ NoCut(WS0) ~ variations("second") map(_.toInt.seconds)) |
      (UnsignedInt.filter(_.toInt <= 59) ~ NoCut(WS0) ~ variations("minute") map (_.toInt.minutes)) |
      (UnsignedInt.filter(_.toInt <= 23) ~ NoCut(WS0) ~ variations("hour") map (_.toInt.hours))
  )
}

object BasicRules extends BasicRules
