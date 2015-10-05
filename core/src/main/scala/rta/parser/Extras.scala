package rta.parser

import scala.language.implicitConversions
import fastparse.Implicits.Sequencer
import fastparse.WhitespaceApi
import fastparse.all._
import fastparse.core.ParseCtx

trait Extras {
  lazy val white = Extras.white

  implicit def toKeyword(keyword: String): Extras.Keyword =
    new Extras.Keyword(keyword)

  implicit def toParserExtras[T](parser: Parser[T]): Extras.ParserExtras[T] =
    new Extras.ParserExtras[T](parser)
}

object Extras {
  implicit class Keyword(val keyword: String) extends AnyVal {
    def withWS: Parser[Unit] = keyword ~ NoCut(Skip1)

    def splitWS: Parser[Unit] = {
      val splitted = keyword.split("\\s+")
      splitted.tail.foldLeft[P[Unit]](splitted.head) { _ ~ NoCut(WS1) ~ _ }
    }

    def push[T](value: T): Parser[T] = wspStr(keyword).map(_ => value)
  }

  implicit class ParserExtras[T](val parser: Parser[T]) extends AnyVal {
    def withFilter(p: T => Boolean) = parser.filter(p)

    def whole: P[T] = P(Start ~ parser ~ End)

    def ~~![V, R](p: P[V])(implicit ev: Sequencer[T, V, R]): P[R] = parser ~! p

    def withWS: Parser[T] = parser ~  NoCut(Skip1)

    def push[U](value: U): Parser[U] = parser.map(_ => value)
  }

  def WS = NoTrace(" " | "\n")

  def Comment = {
    val inComment = new Parser[Unit] {
      def parseRec(cfg: ParseCtx, index: Int) = {
        var curr = index
        val input = cfg.input
        while(curr < input.length - 1 && input(curr) != '*' && input(curr + 1) != '/') {
          input(curr + 1) match {
            case '*' => curr += 1
            case _ => curr += 2
          }
        }
        success(cfg.success, (), curr, Nil, false)
      }
    }
    val MultiLine = "/*" ~ inComment ~ "*/"
    val SingleLine = "//" ~ CharsWhile(_ != '\n', min = 0)
    NoTrace((MultiLine | SingleLine).rep(1))
  }

  def WS0 = NoTrace(WS.rep)

  def WS1 = P(WS.rep(1))("at least one whitespace")

  def Skip0 = NoTrace(WS0 ~ (Comment ~ WS0).rep)

  def Skip1 = P(WS1 ~ (Comment ~ WS0).rep)("at least one whitespace")

  lazy val white = WhitespaceApi.Wrapper(Skip0)
}
