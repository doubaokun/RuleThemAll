package sta.parser

import fastparse.WhitespaceApi
import fastparse.all._

trait WhitespaceSkip {
  def WL = P(NoTrace(" " | "\n" | "\r\n").rep)("WL")

  lazy val white = WhitespaceApi.Wrapper(WL)
}
