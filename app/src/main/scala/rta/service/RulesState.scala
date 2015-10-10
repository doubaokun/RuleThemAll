package rta.service

import rta.concurrent.Atomic
import rta.logging.Logging
import rta.model.{Rule, BaseModel, BaseModelCompanion, ModelKV}
import rta.storage.RulesStorage
import shapeless.HMap

class RulesState(implicit storage: RulesStorage) extends Logging {
  private[this] val inner: Atomic[Map[Any, Any]] = Atomic(Map.empty)
  private[this] val network = ExecutionNetwork(storage.rules)

  def update[V <: BaseModel](model: V)(implicit ctx: RulesExecutor): TraversableOnce[Rule] = {
    import model.companion

    inner.update(state => state.get(companion.Key) match {
      case Some(other) => state + (companion-> model.mergeTo(other.asInstanceOf[model.companion.R[model.Self]]))
      case None => state + (companion.Key -> model.lift)
    }).fold(
      th => {
        log.error("Error has occurred during updating state", th)
        Iterator.empty
      },
      state => ConflictSetResolution.default.resolve(network.activate(model))
    )
  }

  // FIXME
  def reset(): Unit = {
    network.compile(storage.rules)
    network.feed(values)
  }

  // FIXME
  def values: Iterator[BaseModel] = inner.get.valuesIterator.flatMap {
    case xs: List[_] => xs.asInstanceOf[List[BaseModel]]
    case x: BaseModel => List(x)
  }
  
  def snapshot: HMap[ModelKV] = HMap.empty(inner.get)
}
