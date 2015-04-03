package sta.parser

import org.parboiled2._

import scala.language.implicitConversions

abstract class RichParser extends Parser with BasicRules {
  def endWS(s: String): Rule0 = rule(atomic(str(s)) ~ quiet(Whitespace.*))

  implicit def ignoreWS(s: String): Rule0 = rule(atomic(str(s)) ~ quiet(Space.+))

  implicit def ignoreWS(c: Char): Rule0 = rule(quiet(Whitespace.*) ~ ch(c) ~ quiet(Whitespace.*))
}
