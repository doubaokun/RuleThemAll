package sta.model.triggers.functions

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox
import spire.algebra.Order
import sta.model.BaseModel

/** Base for all functions that predicates over [[BaseModel]]. */
abstract class ModelFunction[M <: BaseModel] extends (M => Boolean)

/** Special version of model predicate that can be checked using hash code. */
abstract class HashBasedFunction[V, M <: BaseModel] extends ModelFunction[M] {
  def v: V

  final override def hashCode() = v.hashCode()

  final override def equals(o: Any): Boolean = o match {
    case mf: HashBasedFunction[_, _] =>  v.equals(mf.v)
    case _ => false
  }
}

/** Materializes specific [[ModelFunction]] instance from the general predicate.
  *
  * Note that you should only create specific [[ModelFunction]] instances by hand only
  * if you notice that this function returns nonoptimal instances.
  */
object ModelFunction {
  implicit def materializeModelFunction[M <: BaseModel](f: M => Boolean): ModelFunction[M] = macro ModelFunctionImpl.makeInstance[M]
}

private[triggers] class ModelFunctionImpl(val c: blackbox.Context) {
  import c.universe._
  import org.scalamacros.resetallattrs._
  
  // TODO add rewrite to EqualFunction cases like (_.present), where `present: Boolean`
  // TODO add split of multiple ands (`&&`) and ors (`||`) of conditions into atomic ones
  def makeInstance[M <: BaseModel: WeakTypeTag](f: Tree): Tree = {
    val tpeM = weakTypeOf[M]

    def Order(tpe: Type) = q"implicitly[${appliedType(typeOf[Order[_]].typeConstructor, tpe.widen)}]"

    def ModelFunction2(base: Type, tpe: Type)  = appliedType(base.typeConstructor, tpe.widen, tpeM)

    def ModelFunction1(base: Type) = appliedType(base.typeConstructor, tpeM)

    f.duplicate match {
      case q"(..$params) => $lhs == $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpeM"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"new ${ModelFunction2(typeOf[EqualFunction[_, _]], rhs.tpe)}($rhs)($conv)"

      case q"(..$params) => $lhs != $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpeM"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"new ${ModelFunction2(typeOf[NotEqualFunction[_, _]], rhs.tpe)}($rhs)($conv)"

      case q"(..$params) => $lhs > $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpeM"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"new ${ModelFunction2(typeOf[GTFunction[_, _]], rhs.tpe)}($rhs)(${Order(rhs.tpe)}, $conv)"
                                                  
      case q"(..$params) => $lhs >= $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpeM"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"new ${ModelFunction2(typeOf[GTEQFunction[_, _]], rhs.tpe)}($rhs)(${Order(rhs.tpe)}, $conv)"

      case q"(..$params) => $lhs < $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpeM"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"new ${ModelFunction2(typeOf[LTFunction[_, _]], rhs.tpe)}($rhs)(${Order(rhs.tpe)}, $conv)"

      case q"(..$params) => $lhs <= $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpeM"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"new ${ModelFunction2(typeOf[LTEQFunction[_, _]], rhs.tpe)}($rhs)(${Order(rhs.tpe)}, $conv)"

      case q"(..$params) => !$v" =>
        q"new ${ModelFunction1(typeOf[NotFunction[_]])}(${makeInstance[M](q"(..$params) => $v")})"

      case other =>
        q"new ${ModelFunction1(typeOf[ModelFunction[_]])} { def apply(m: $tpeM) = $other(m) } "
    }
  }
}
