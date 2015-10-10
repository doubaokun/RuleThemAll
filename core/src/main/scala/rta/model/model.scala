package rta.model

import scala.language.higherKinds
import enumeratum.{Enum, EnumEntry}
import java.lang.{Enum => JEnum}

/** Marker for specific models in the state map. */
class ModelKV[+K, +V]

/** Base for all models that are part of the rules state. */
sealed abstract class BaseModel(val companion: BaseModelCompanion[BaseModel]) extends Serializable { self =>
  /** Recursive type alias that denotes self type.
    *
    * Note that this notation is weaker than using recursive generic param, but since this class
    * it sealed and its implementations are using generics anyway that shouldn't matter.
    * In return we get rid of writing BaseModel[_] in other parts of code.
    */
  type Self <: BaseModel { type Self = self.Self }

  def lift: companion.R[Self]

  def mergeTo(seq: companion.R[Self]): companion.R[Self]

  implicit def ev: ModelKV[companion.Key.type, companion.R[Self]] = new ModelKV
}

/** Denotes model that have single, or no instance in the rules state. */
abstract class Model[M <: Model[M]](override val companion: ModelCompanion[M])
  extends BaseModel(companion) { self: M =>
  final type Self = M

  def lift: companion.R[M] = this

  def mergeTo(seq: companion.R[Self]): companion.R[Self] = this
}

/** Denotes model that can have multiple instances in the rules state. */
abstract class MultiModel[M <: MultiModel[M]](override val companion: MultiModelCompanion[M])
  extends BaseModel(companion) { self: M =>
  final type Self = M

  def lift: companion.R[M] = List(this)

  def mergeTo(seq: companion.R[Self]): companion.R[Self] = {
    val (_, base) = seq.partition(_.sameAs(this))
    base :+ this
  }

  def sameAs(other: M): Boolean
}

sealed abstract class BaseModelCompanion[+M <: BaseModel] { root =>
  type R[+S]

  def exists[S >: M <: BaseModel](s: R[S], p: S => Boolean): Boolean

  case object Key {
    // overriding because it's indistinguishable from `Key` in different companion
    override def hashCode(): Int = root.hashCode()

    // overriding together with `hashCode`, but it seems to work correctly with default implementation
    override def equals(o: Any): Boolean = o match {
      case _: this.type => true
      case _ => false
    }
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
