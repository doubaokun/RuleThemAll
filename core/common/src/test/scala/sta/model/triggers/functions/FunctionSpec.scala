package sta.model.triggers.functions

import org.scalatest.{ Matchers, WordSpec }
import spire.implicits._
import sta.model.ModelHelpers
import sta.tests.PropertyChecks

import scalaz.Scalaz.{ ToEqualOps ⇒ _ }

class FunctionSpec extends WordSpec with PropertyChecks with Matchers with ModelHelpers {

  import ops._

  "Testing equality" should {
    "behave like equality operators" when {
      "comparing wrapped primitives" in {
        forAll(intTupleGen) {
          case (v: Int, w: IntWrapper) ⇒
            whenever(v == w.v) {
              EqualFunction[IntWrapper](v).apply(w) should be === true
              NotEqualFunction[IntWrapper](v).apply(w) should be === false
            }
        }

        forAll(intTupleGen) {
          case (v: Int, w: IntWrapper) ⇒
            whenever(v != w.v) {
              EqualFunction[IntWrapper](v).apply(w) should be === false
              NotEqualFunction[IntWrapper](v).apply(w) should be === true
            }
        }
      }

      "comparing wrapped references" in {
        forAll(stringTupleGen) {
          case (v: String, w: StringWrapper) ⇒
            whenever(v == w.v) {
              EqualFunction[StringWrapper](v).apply(w) should be === true
              NotEqualFunction[StringWrapper](v).apply(w) should be === false
            }
        }

        forAll(stringTupleGen) {
          case (v: String, w: StringWrapper) ⇒
            whenever(v != w.v) {
              EqualFunction[StringWrapper](v).apply(w) should be === false
              NotEqualFunction[StringWrapper](v).apply(w) should be === true
            }
        }
      }

      "comparing objects" in {
        forAll(objectGen, objectGen) {
          case (v: Object, w: Object) ⇒
            whenever(v == w) {
              EqualFunction(v).apply(w) should be === true
              NotEqualFunction(v).apply(w) should be === false
            }
        }

        forAll(objectGen, objectGen) {
          case (v: Object, w: Object) ⇒
            whenever(v != w) {
              EqualFunction(v).apply(w) should be === false
              NotEqualFunction(v).apply(w) should be === true
            }
        }
      }
    }
  }

  "Testing ordering" should {
    "behave like ordering operators" when {
      "comparing wrapped primitives" in {
        forAll(intGen, intGen) {
          case (v: Int, w: Int) ⇒
            whenever(v > w.v) {
              LTFunction[Int, IntWrapper](v).apply(w) should be === true
              GTEQFunction[Int, IntWrapper](v).apply(w) should be === false
            }
        }

        forAll(intGen, intGen) { (v: Int, w: Int) ⇒
          whenever(v <= w.v) {
            LTFunction[Int, IntWrapper](v).apply(w) should be === false
            GTEQFunction[Int, IntWrapper](v).apply(w) should be === true
          }
        }

        forAll(intGen, intGen) { (v: Int, w: Int) ⇒
          whenever(v < w.v) {
            GTFunction[Int, IntWrapper](v).apply(w) should be === true
            LTEQFunction[Int, IntWrapper](v).apply(w) should be === false
          }
        }

        forAll(intGen, intGen) { (v: Int, w: Int) ⇒
          whenever(v >= w.v) {
            GTFunction[Int, IntWrapper](v).apply(w) should be === false
            LTEQFunction[Int, IntWrapper](v).apply(w) should be === true
          }
        }
      }

      "comparing wrapped references" in {
        forAll(stringGen, stringGen) {
          case (v: String, w: String) ⇒
            whenever(v > w.v) {
              LTFunction[String, StringWrapper](v).apply(w) should be === true
              GTEQFunction[String, StringWrapper](v).apply(w) should be === false
            }
        }

        forAll(stringGen, stringGen) {
          case (v: String, w: String) ⇒
            whenever(v <= w.v) {
              LTFunction[String, StringWrapper](v).apply(w) should be === false
              GTEQFunction[String, StringWrapper](v).apply(w) should be === true
            }
        }

        forAll(stringGen, stringGen) { (v: String, w: String) ⇒
          whenever(v < w.v) {
            GTFunction[String, StringWrapper](v).apply(w) should be === true
            LTEQFunction[String, StringWrapper](v).apply(w) should be === false
          }
        }

        forAll(stringGen, stringGen) { (v: String, w: String) ⇒
          whenever(v >= w.v) {
            GTFunction[String, StringWrapper](v).apply(w) should be === false
            LTEQFunction[String, StringWrapper](v).apply(w) should be === true
          }
        }
      }
    }
  }

}
