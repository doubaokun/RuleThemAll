//package sta.rules.engine
//
//import scala.collection.mutable
//import sta.rules.engine.condition.Condition
//import sta.rules.engine.utils.Hash
//
//
//private[rules] class AlphaNetwork {
//  private val _hashBased = mutable.LongMap.empty[AlphaMemory.HashBased]
//  private val _booleanBased = mutable.ArrayBuffer.empty[AlphaMemory.BooleanBased]
//
//  def +=[T <: Condition[_]](node: AlphaMemory[T]): this.type = {
//    node match {
//      case n: AlphaMemory.HashBased => _hashBased += (n.hash, n)
//      case n: AlphaMemory.BooleanBased => _booleanBased += n
//    }
//
//    this
//  }
//
//  def +=[T: Hash](wme: T): this.type = {
//    _hashBased.get(implicitly[Hash[T]].hash).foreach(_ - wme)
//    _hashBased.get(Hash.neutral).foreach(_ - wme)
//
//    for (node <- _booleanBased if node.matches(wme)) {
//      node + wme
//    }
//
//    this
//  }
//
//  def -=[T: Hash](wme: T): this.type = {
//    _hashBased.get(implicitly[Hash[T]].hash).foreach(_ + wme)
//    _hashBased.get(Hash.neutral).foreach(_ + wme)
//
//    // TODO optimize
//    for (node <- _booleanBased if node.matches(wme)) {
//      node - wme
//    }
//
//    this
//  }
//}
