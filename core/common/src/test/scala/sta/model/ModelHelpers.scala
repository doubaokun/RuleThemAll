package sta.model

import scala.language.implicitConversions
import org.scalacheck.Gen
import shapeless.HMap
import spire.algebra.Order
import sta.common.category
import sta.model.triggers.functions._

trait ModelHelpers {

  @category("test")
  case class TestModel(i: Int = 0, s: String = "", o: TestModel.O = null) extends Model(TestModel)
  implicit object TestModel extends ModelCompanion[TestModel] {
    sealed abstract class O

    object O {
      object Object1 extends O
      object Object2 extends O
    }
  }

  object ops {
    import TestModel._

    implicit def unwrapInt(from: TestModel): Int = from.i

    implicit def unwrapString(from: TestModel): String = from.s

    implicit def unwrapObject(from: TestModel): O = from.o

    implicit val intGen: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

    implicit val stringGen: Gen[String] = Gen.alphaStr

    implicit val oGen: Gen[O] = Gen.oneOf(O.Object1, O.Object2)

    implicit val testGen: Gen[TestModel] = for {
      i <- intGen
      s <- stringGen
      o <- oGen
    } yield TestModel(i, s, o)

    implicit def tupledGen[T](g: Gen[T])(implicit unwrap: TestModel => T): Gen[(T, TestModel)] = {
      Gen.oneOf(testGen.map(t => (unwrap(t), t)), Gen.zip(g, testGen))
    }

    implicit def modelFunctionGen[T: Gen: Order](implicit ev: TestModel => T): Gen[ModelFunction[TestModel]] =
      implicitly[Gen[T]].flatMap { v =>
        Gen.oneOf[ModelFunction[TestModel]](
          EqualFunction[T, TestModel](v),
          NotEqualFunction[T, TestModel](v),
          LTFunction[T, TestModel](v),
          LTEQFunction[T, TestModel](v),
          GTFunction[T, TestModel](v),
          GTEQFunction[T, TestModel](v)
        )
      }

    implicit def stateGen[T]: Gen[HMap[ModelKV]] = {
      testGen.map { m =>
        HMap[ModelKV](m.companion.Key -> m.asInstanceOf[Model])(m.companion.ev)
      }
    }
  }

}

