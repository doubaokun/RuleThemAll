package sta.model

class ModelKV[+K, +V]

abstract class Model(val companion: ModelCompanion[Model])

abstract class ModelCompanion[+M <: Model] { root ⇒
  case object Key {
    override def hashCode(): Int = root.hashCode() // in order to be properly handled in HMap
  }

  implicit def ev: ModelKV[Key.type, M] = new ModelKV
}
