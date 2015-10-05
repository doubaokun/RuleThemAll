package rta.model.triggers.functions

import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import spire.algebra.Order
import spire.implicits._
import rta.model.ModelHelpers
import rta.tests.PropertyChecks

class ModelFunctionSpec extends FlatSpec with PropertyChecks with Matchers with ModelHelpers {

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

  behavior of "ModelFunction"

  it should "implement correct equality operators" in {
    testFunctions[Int](intGen, EqualFunction.apply _, NotEqualFunction.apply _)(_ == _)
    testFunctions[String](stringGen, EqualFunction.apply _, NotEqualFunction.apply _)(_ == _)
    testFunctions[TestModel.O](oGen, EqualFunction.apply _, NotEqualFunction.apply _)(_ == _)
  }

  it should "implement correct ordering operators" in {
    testFunctions[Int](intGen, LTFunction.apply _, GTEQFunction.apply _)(_ > _)
    testFunctions[Int](intGen, GTFunction.apply _, LTEQFunction.apply _)(_ < _)

    testFunctions[String](stringGen, LTFunction.apply _, GTEQFunction.apply _)(_ > _)
    testFunctions[String](stringGen, GTFunction.apply _, LTEQFunction.apply _)(_ < _)
  }

  it should "be constructed from plain Scala function" in {
    import ModelFunction.{materializeModelFunction => mat}
    val test = TestModel(1, "abc", TestModel.O.Object1)
    val o = implicitly[Order[Int]]

    mat[TestModel](_ == test) should === (EqualFunction[TestModel, TestModel](test))
    mat[TestModel](_ != test) should === (NotEqualFunction[TestModel, TestModel](test))
    mat[TestModel](_.s.==("test")) should ===(EqualFunction[String, TestModel]("test")(_.s))
    mat[TestModel](v => v.o != test.o) should ===(NotEqualFunction[TestModel.O, TestModel](test.o)(_.o))
    mat[TestModel](_.i > 1) should ===(GTFunction[Int, TestModel](1)(o, _.i))
    mat[TestModel](_.i >= (6 / 2)) should ===(GTEQFunction[Int, TestModel](3)(o, _.i))
    mat[TestModel](_.i < 1/12) should ===(LTFunction[Int, TestModel](1/12)(o, _.i))
    mat[TestModel](_.i <= math.pow(2, 2).toInt) should ===(LTEQFunction[Int, TestModel](4)(o, _.i))
    mat[TestModel](v => !(v.s == "test")) should ===(NotFunction[TestModel](EqualFunction[String, TestModel]("test")(_.s)))

    mat[TestModel](_.i % 2 == 1).apply(TestModel(3)) should === (true)
    mat[TestModel](_.i % 2 == 1).apply(TestModel(4)) should === (false)
  }

}
