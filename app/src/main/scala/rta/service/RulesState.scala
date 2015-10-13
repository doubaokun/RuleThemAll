package rta.service

import rta.concurrent.Atomic
import rta.logging.Logging
import rta.model.{Rule, BaseModel, BaseModelCompanion, ModelKV}
import rta.storage.RulesStorage
import shapeless.HMap

class RulesState(implicit storage: RulesStorage) extends Logging {
  private[this] val inner: Atomic[HMap[ModelKV]] = Atomic(HMap.empty)
//  private[this] val network = ExecutionNetwork(storage.rules)

  def update[V <: BaseModel](model: V)(implicit ctx: RulesExecutor): TraversableOnce[Rule] = {
    import model.companion

    inner.update(state => state.get(companion.Key)(model.ev) match {
      case Some(other) => state.+(companion.Key -> model.mergeTo(other))(model.ev)
      case None => state.+(companion.Key -> model.lift)(model.ev)
    }).fold(
      th => {
        log.error("Error has occurred during updating state", th)
        Iterator.empty
      },
      state => {
//        ConflictSetResolution.default.resolve(network.activate(model))
        ConflictSetResolution.default.resolve(storage.rules.filter(_.satisfiedBy(inner.get)))
      }
    )
  }

  // FIXME
  def reset(): Unit = {
//    network.compile(storage.rules)
//    network.feed(values)
  }

  // FIXME
//  def values: Iterator[BaseModel] = inner.get.valuesIterator.flatMap {
//    case xs: List[_] => xs.asInstanceOf[List[BaseModel]]
//    case x: BaseModel => List(x)
//  }
  
  def snapshot: HMap[ModelKV] = inner.get
}
