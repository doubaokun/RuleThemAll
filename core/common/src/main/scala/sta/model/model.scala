package sta.model

import android.content.Context
import enumeratum.{EnumEntry , Enum}

class ModelKV[+K, +V]

abstract class Model(val companion: ModelCompanion[Model])

abstract class ModelCompanion[+M <: Model] { root =>

  case object Key {
    override def hashCode(): Int = root.hashCode() // in order to be properly handled in HMap
  }

  implicit def ev: ModelKV[Key.type, M] = new ModelKV
}

//case class MultiModel[M <: Model](models: Set[M])
//  (implicit override val companion: MultiModelCompanion[M]) extends Model(companion) {
//  def +(model: M): MultiModel[M] = copy[M](models = models + model)
//
//  def -(model: M): MultiModel[M] = copy[M](models = models - model)
//}
//
//object MultiModel {
//  def empty[M <: Model](implicit companion: MultiModelCompanion[M]) = MultiModel(Set.empty)
//}
//
//abstract class MultiModelCompanion[M <: Model] extends ModelCompanion[MultiModel[M]]

trait ModelEnumEntry extends EnumEntry with Product {
  override def entryName: String = productPrefix.replaceAll("(\\p{Ll})(\\p{Lu})", "$1_$2").toLowerCase
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
