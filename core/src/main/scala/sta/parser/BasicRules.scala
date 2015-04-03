package sta.parser

import org.parboiled2.CharPredicate._
import org.parboiled2._
import spire.math.{ Natural ⇒ Nat, Rational, SafeLong, UByte, UInt }

import scala.util.Try

trait BasicRules { this: Parser ⇒
  def Percent: Rule1[UByte] = rule(
    capture('0' | ('1' ~ '0' ~ '0') | (Digit19 ~ Digit.?)) ~ '%' ~> ((s: String) ⇒ UByte(s.toByte))
  )

  def Byte: Rule1[UByte] = rule(
    ("0x" ~ capture(HexDigit.+) ~> ((s: String) ⇒ {
      val v = Try(java.lang.Integer.parseInt(s, 16))
      test(v.isSuccess && v.get <= 255) ~ push(UByte(v.get))
    })) | (capture(('0' ~ !'x') | (Digit19 ~ Digit.*)) ~> ((s: String) ⇒ {
      val v = Try(s.toInt)
      test(v.isSuccess && v.get <= 255) ~ push(UByte(v.get))
    }))
  )

  def UnsignedInt: Rule1[UInt] = rule(
    (capture(('0' ~ !'x') | (Digit19 ~ Digit.*)) ~> ((s: String) ⇒ {
      val v = Try(s.toLong)
      test(v.isSuccess && v.get <= 4294967295L) ~ push(UInt(v.get))
    })) | ("0x" ~ capture(HexDigit.+) ~> ((s: String) ⇒ {
      val v = Try(java.lang.Long.parseLong(s, 16))
      test(v.isSuccess && v.get <= 4294967295L) ~ push(UInt(v.get))
    }))
  )

  def Natural: Rule1[Nat] = rule(
    capture('0' | (Digit19 ~ Digit.*)) ~> (Nat(_: String))
  )

  def Int: Rule1[Int] = rule(
    capture('-'.? ~ ('0' | (Digit19 ~ Digit.*))) ~> ((s: String) ⇒ {
      val v = Try(s.toInt)
      test(v.isSuccess) ~ push(v.get)
    })
  )

  def Integer: Rule1[SafeLong] = rule(
    capture('-'.? ~ ('0' | (Digit19 ~ Digit.*))) ~> (SafeLong(_: String))
  )

  def Float: Rule1[Float] = rule(
    capture('-'.? ~ Digit.+ ~ ('.' ~ Digit.+).?) ~> ((s: String) ⇒ {
      val v = Try(s.toDouble)
      test(v.isSuccess && {
        val vv = v.get
        vv <= scala.Float.MaxValue && vv >= scala.Float.MinValue
      }) ~ push(v.get.toFloat)
    })
  )

  def Decimal: Rule1[Rational] = rule(
    capture('-'.? ~
      ((Digit.+ ~ '/' ~ (Digit19 ~ Digit.*)) |
        (Digit.+ ~ ('.' ~ Digit.+ ~ ('E' ~ '-'.? ~ Digit.+).?).?))
    ) ~> (Rational(_: String))
  )

  // String literal rules taken from paulp/scala-parser
  private def Quote = rule('"')
  private def SingleChar = rule("\\\"" | "\\\\" | noneOf("\n\""))
  private def SingleString: Rule1[String] = rule(Quote ~ capture(SingleChar.*) ~ Quote)

  private final val TQ = "\"\"\""
  private def OptQuote = rule(Quote.?)
  private def NonQuoteChar = rule(!Quote ~ ANY)
  private def TripleChar = rule(OptQuote ~ OptQuote ~ NonQuoteChar)
  private def TripleEnd = rule(TQ ~ ch('"').*)
  private def TripleStart = rule(TQ ~ (!TQ ~ '"').*)
  private def TripleString: Rule1[String] = rule(TripleStart ~ capture(TripleChar.*) ~ TripleEnd)

  def String: Rule1[String] = rule(TripleString | SingleString)

  def Space: Rule0 = rule(' ')

  def Newline: Rule0 = rule('\r'.? ~ '\n')

  def Whitespace: Rule0 = rule(Space | Newline)

  def Newline1: Rule0 = rule(Space.* ~ Newline ~ Whitespace.*)
}
