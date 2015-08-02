package sta.model.triggers.functions

import org.scalacheck.Gen
import org.scalatest.{ Matchers, WordSpec }
import spire.implicits._
import sta.model.ModelHelpers
import sta.tests.PropertyChecks

class FunctionSpec extends WordSpec with PropertyChecks with Matchers with ModelHelpers {

  import ops._

  private def testFunctions[V](gen: Gen[V], f1: V => ModelFunction[TestModel],
                               f2: V => ModelFunction[TestModel])(pred: (V, V) => Boolean)(implicit unwrap: TestModel => V): Unit = {
    forAll(tupledGen(gen)) {
      case (v, m) =>
        whenever(pred(v, m)) {
          f1(v).apply(m) should ===(true)
          f2(v).apply(m) should ===(false)
        }
    }

    forAll(tupledGen(gen)) {
      case (v, m) =>
        whenever(!pred(v, m)) {
          f1(v).apply(m) should ===(false)
          f2(v).apply(m) should ===(true)
        }
    }
  }

  "Testing equality" should {
    "behave like equality operators" in {
      testFunctions[Int](intGen, EqualFunction.apply _, NotEqualFunction.apply _)(_ == _)
      testFunctions[String](stringGen, EqualFunction.apply _, NotEqualFunction.apply _)(_ == _)
      testFunctions[TestModel.O](oGen, EqualFunction.apply _, NotEqualFunction.apply _)(_ == _)
    }
  }

  "Testing ordering" should {
    "behave like ordering operators" in {
      testFunctions[Int](intGen, LTFunction.apply _, GTEQFunction.apply _)(_ > _)
      testFunctions[Int](intGen, GTFunction.apply _, LTEQFunction.apply _)(_ < _)

      testFunctions[String](stringGen, LTFunction.apply _, GTEQFunction.apply _)(_ > _)
      testFunctions[String](stringGen, GTFunction.apply _, LTEQFunction.apply _)(_ < _)
    }
  }

}
