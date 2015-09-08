package sta.model

import scala.language.higherKinds
import enumeratum.{EnumEntry , Enum}
import java.lang.{Enum => JEnum}

class ModelKV[+K, +V]

sealed abstract class BaseModel(val companion: BaseModelCompanion[BaseModel]) { self =>
  type Self <: BaseModel { type Self = self.Self }

  def lift: companion.R[Self]

  def mergeTo(seq: companion.R[Self]): companion.R[Self]

  implicit def ev: ModelKV[companion.Key.type, companion.R[Self]] = new ModelKV
}

abstract class Model[M <: Model[M]](override val companion: ModelCompanion[M])
  extends BaseModel(companion) { self: M =>
  type Self = M

  def lift: companion.R[M] = this

  def mergeTo(seq: companion.R[Self]): companion.R[Self] = this
}

abstract class MultiModel[M <: MultiModel[M]](override val companion: MultiModelCompanion[M])
  extends BaseModel(companion) { self: M =>
  type Self = M

  def lift: companion.R[M] = List(this)

  def mergeTo(seq: companion.R[Self]): companion.R[Self] = {
    val (_, base) = seq.partition(_.sameAs(this))
    base :+ this
  }

  def sameAs(other: M): Boolean
}

sealed abstract class BaseModelCompanion[+M <: BaseModel] { root =>
  type R[+S]// <: {def exists(p: S => Boolean): Boolean}

  def exists[S >: M <: BaseModel](s: R[S], p: S => Boolean): Boolean

  case object Key {
    override def hashCode(): Int = root.hashCode() // in order to be properly handled in HMap
  }

  implicit def ev: ModelKV[Key.type, R[M]] = new ModelKV
}

abstract class ModelCompanion[+M <: Model[_]] extends BaseModelCompanion[M] { root =>
  type Phantom[+S] = S

  type R[+S] = Phantom[S]

  def exists[S >: M <: BaseModel](s: R[S], p: S => Boolean): Boolean = p(s)
}

abstract class MultiModelCompanion[+M <: MultiModel[_]] extends BaseModelCompanion[M] {
  type R[+S] = List[S]

  def exists[S >: M <: BaseModel](s: R[S], p: S => Boolean): Boolean = s.exists(p)
}

trait ModelEnumEntry extends EnumEntry with Product {
  override def entryName: String = productPrefix.replaceAll("(\\p{Ll})(\\p{Lu})", "$1 $2").toLowerCase
}

trait FromIntEntry extends ModelEnumEntry {
  def intValue: Int
}

trait FromInt[T <: FromIntEntry] { this: Enum[T] =>
  lazy val intValues: Map[Int, T] = values.map(e => e.intValue -> e)(collection.breakOut)

  def fromInt(i: Int): T = intValues.getOrElse(
    i, throw new NoSuchElementException(s"$i is not a member of $this")
  )
}

trait FromJavaEnumEntry[E <: JEnum[E]] extends ModelEnumEntry {
  def enumValue: E
}

trait FromJavaEnum[E <: JEnum[E], T <: FromJavaEnumEntry[E]] { this: Enum[T] =>
  lazy val enumValues: Map[E, T] = values.map(e => e.enumValue -> e)(collection.breakOut)

  def fromEnum(e: E): T = enumValues.getOrElse(
    e, throw new NoSuchElementException(s"$e is not a member of $this")
  )
}
