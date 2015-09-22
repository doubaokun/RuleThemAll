package sta.parser

import scala.language.implicitConversions
import fastparse.WhitespaceApi
import fastparse.all._

trait Extras {
  lazy val white = Extras.white

  implicit def toKeyword(keyword: String): Extras.Keyword =
    new Extras.Keyword(keyword)

  implicit def toParserExtras[T](parser: Parser[T]): Extras.ParserExtras[T] =
    new Extras.ParserExtras[T](parser)
}

object Extras {
  class Keyword(val keyword: String) extends AnyVal {
    def withWS: Parser[Unit] = keyword ~ NoCut(WS1)

    def splitWS: Parser[Unit] = {
      val splitted = keyword.split("\\s+")
      splitted.tail.foldLeft[P[Unit]](splitted.head) { _ ~ NoCut(WS1) ~ _ }
    }

    def push[T](value: T): Parser[T] = wspStr(keyword).map(_ => value)
  }

  class ParserExtras[T](val parser: Parser[T]) extends AnyVal {
    def withFilter(p: T => Boolean) = parser.filter(p)

    def withWS: Parser[T] = parser ~ NoCut(WS1)

    def push[U](value: U): Parser[U] = parser.map(_ => value)
  }

  def WS = NoTrace(" " | "\n" | "\r\n")

  def WS0 = P(WS.rep)("\\s*")

  def WS1 = P(WS.rep(1))("\\s+")

  lazy val white = WhitespaceApi.Wrapper(WS0)
}
