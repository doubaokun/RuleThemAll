package sta.parser

import org.scalacheck.Gen

trait ParserHelpers {
  implicit class RichGen(g: Gen.type) {
    import g._

    def floatString(predicate: Double ⇒ Boolean) = {
      val f = for {
        n ← nonEmptyNumStr
        d ← nonEmptyNumStr
        s ← Gen.const(s"$n.$d") if predicate(s.toDouble)
      } yield s
      Gen.oneOf(f, f.map('-' + _))
    }

    def rationalString = {
      val r = for {
        n ← nonEmptyNumStr
        d ← nonEmptyNumStr if d.forall(_ != '0')
      } yield s"$n/$d"
      Gen.oneOf(r, r.map('-' + _))
    }

    def nonEmptyNumStr = nonEmptyListOf(numChar).map(_.mkString).suchThat(_.forall(_.isDigit))

    def nDigitNumStr(n: Int) = listOfN(n, numChar).map(_.mkString).suchThat(_.forall(_.isDigit))
  }
}
