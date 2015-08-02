//package sta.rules.engine
//
//import scala.reflect.ClassTag
//
//
//sealed trait MatchRestriction
//
//sealed trait RestrictionType {
//  this: MatchRestriction =>
//}
//
//trait HashBasedRestriction extends RestrictionType { this: MatchRestriction =>
//  override final def hashCode: Int = hash
//
//  @inline protected def hash: Int
//}
//
//trait BooleanBasedRestriction extends RestrictionType { this: MatchRestriction =>
//  override final def equals(o: scala.Any): Boolean = matches(o)
//
//  @inline protected def matches(o: scala.Any): Boolean
//}
//
//trait ObjectRestriction extends MatchRestriction
//
//trait AttributeRestriction extends MatchRestriction
//
//trait ValueRestriction extends MatchRestriction
//
//object NoRestriction extends ObjectRestriction with AttributeRestriction with ValueRestriction with HashBasedRestriction with BooleanBasedRestriction {
//  @inline protected def hash: Int = 0
//
//  @inline protected def matches(o: scala.Any): Boolean = true
//}
//
//class TypeRestriction[T: ClassTag] extends ObjectRestriction with HashBasedRestriction with BooleanBasedRestriction {
//  @inline protected def hash: Int = implicitly[ClassTag[T]].hashCode()
//
//  @inline protected def matches(o: scala.Any): Boolean = o match {
//    case t: T => true
//    case _    => false
//  }
//}
//
//class FieldRestriction(private val fieldName: String) extends AttributeRestriction with HashBasedRestriction with BooleanBasedRestriction {
//  @inline protected def hash: Int = fieldName.hashCode
//
//  @inline protected def matches(o: scala.Any): Boolean = o match {
//    case other: String => fieldName == other
//    case _             => false
//  }
//}
