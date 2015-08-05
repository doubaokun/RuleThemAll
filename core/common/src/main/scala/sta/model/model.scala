package sta.model

import enumeratum.{EnumEntry , Enum}

class ModelKV[+K, +V]

abstract class Model(val companion: ModelCompanion[Model])

abstract class ModelCompanion[+M <: Model] { root =>

  case object Key {
    override def hashCode(): Int = root.hashCode() // in order to be properly handled in HMap
  }

  implicit def ev: ModelKV[Key.type, M] = new ModelKV
}

trait ModelEnumEntry extends EnumEntry with Product {
  override def entryName: String = productPrefix.toLowerCase.replace(' ', '_')
}

trait FromIntEntry extends ModelEnumEntry {
  def intValue: Int
}

trait FromInt[T <: FromIntEntry] { this: Enum[T] =>
  private lazy val intValues: Map[Int, T] = values.map(e => e.intValue -> e)(collection.breakOut)

  def fromInt(i: Int): T = intValues.getOrElse(
    i, throw new NoSuchElementException(s"$i is not a member of $this")
  )
}
