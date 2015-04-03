package sta.model

import kj.android.common.category
import org.scalacheck.Gen
import shapeless.HMap
import spire.algebra.Order
import sta.model.triggers.functions._

import scala.language.implicitConversions
import scalaz.Equal

//@generateCodecs
trait ModelHelpers {
  implicit object IntWrapper extends ModelCompanion[IntWrapper]
  @category("test") case class IntWrapper(v: Int) extends Model(IntWrapper)
  implicit def toWrapper(v: Int): IntWrapper = IntWrapper(v)
  implicit def toUnderlying(w: IntWrapper): Int = w.v

  implicit object StringWrapper extends ModelCompanion[StringWrapper]
  @category("test") case class StringWrapper(v: String) extends Model(StringWrapper)
  implicit def toWrapper(v: String): StringWrapper = StringWrapper(v)
  implicit def toUnderlying(w: StringWrapper): String = w.v

  implicit object Object extends ModelCompanion[Object] {
    object Object1 extends Object
    object Object2 extends Object
  }
  sealed abstract class Object extends Model(Object)

  object ops {
    import Object._

    implicit def iwE: Equal[IntWrapper] = Equal.equalA

    implicit def swE: Equal[StringWrapper] = Equal.equalA

    implicit def owE: Equal[Object] = Equal.equalA

    private def tupleGen[T1 <% T2, T2](single: Gen[T1]): Gen[(T1, T2)] = {
      single.flatMap { v1 ⇒
        val v2: T2 = v1
        Gen.oneOf(Gen.const(v1 -> v2), Gen.zip(single, Gen.const(v2)))
      }
    }

    implicit val intGen: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

    implicit val intTupleGen: Gen[(Int, IntWrapper)] = tupleGen(intGen)

    implicit val stringGen = Gen.alphaStr

    implicit val stringTupleGen: Gen[(String, StringWrapper)] = tupleGen(stringGen)

    implicit val objectGen = Gen.oneOf(Object1, Object2)

    implicit def modelFunctionGen[T <% M: Gen: Order, M <: Model <% T: Equal]: Gen[ModelFunction[M]] =
      implicitly[Gen[T]].flatMap { v ⇒
        Gen.oneOf[ModelFunction[M]](
          EqualFunction[M](v),
          NotEqualFunction[M](v),
          LTFunction[T, M](v),
          LTEQFunction[T, M](v),
          GTFunction[T, M](v),
          GTEQFunction[T, M](v)
        )
      }

    implicit def stateGen[T <% M: Gen, M <: Model]: Gen[HMap[ModelKV]] = {
      implicitly[Gen[T]].map { v ⇒
        val m: M = v
        import m.companion._
        HMap[ModelKV](Key -> m.asInstanceOf[Model])(ev)
      }
    }
  }
}

