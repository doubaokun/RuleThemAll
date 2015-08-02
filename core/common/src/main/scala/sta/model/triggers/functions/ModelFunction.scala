package sta.model.triggers.functions

import scala.language.experimental.macros
import scala.language.implicitConversions

import scala.reflect.macros.blackbox
import sta.model.Model

abstract class ModelFunction[M <: Model] extends (M => Boolean)

abstract class HashBasedFunction[V, M <: Model] extends ModelFunction[M] {
  def v: V

  final override def hashCode() = v.hashCode()
}

object ModelFunction {
  implicit def materializeModelFunction[M <: Model](f: M => Boolean): ModelFunction[M] = macro ModelFunctionImpl.makeInstance[M]
}

private[triggers] class ModelFunctionImpl(val c: blackbox.Context) {
  import c.universe._
  import org.scalamacros.resetallattrs._

  // TODO add rewrite to EqualFunction cases like:
  // (_.present), where `present: Boolean`
  def makeInstance[M <: Model: WeakTypeTag](f: c.Expr[M => Boolean]) = {
    val tpe = weakTypeOf[M]
    
    f.tree.duplicate match {
      case q"(..$params) => $lhs == $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpe"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"new sta.model.triggers.functions.EqualFunction[${rhs.tpe.widen}, $tpe]($rhs)($conv)"

      case q"(..$params) => $lhs != $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpe"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"new sta.model.triggers.functions.NotEqualFunction[${rhs.tpe.widen}, $tpe]($rhs)($conv)"

      case q"(..$params) => $lhs > $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpe"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"""new sta.model.triggers.functions.GTFunction[${rhs.tpe.widen}, $tpe]($rhs)(
              implicitly[spire.algebra.Order[${rhs.tpe.widen}]], $conv)
         """

      case q"(..$params) => $lhs >= $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpe"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"""new sta.model.triggers.functions.GTEQFunction[${rhs.tpe.widen}, $tpe]($rhs)(
              implicitly[spire.algebra.Order[${rhs.tpe.widen}]], $conv)
         """

      case q"(..$params) => $lhs < $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpe"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"""new sta.model.triggers.functions.LTFunction[${rhs.tpe.widen}, $tpe]($rhs)(
              implicitly[spire.algebra.Order[${rhs.tpe.widen}]], $conv)
         """

      case q"(..$params) => $lhs <= $rhs" =>
        val List(q"$_ val $n: $_") = params
        val arg = q"$n: $tpe"
        val conv = c.resetAllAttrs(q"($arg => $lhs)")
        q"""new sta.model.triggers.functions.LTEQFunction[${rhs.tpe.widen}, $tpe]($rhs)(
              implicitly[spire.algebra.Order[${rhs.tpe.widen}]], $conv)
         """

      case q"(..$params) => !$v" =>
        val List(q"$_ val $_: $_") = params
        q"new sta.model.triggers.functions.NotFunction[${v.tpe.widen}, $tpe]($v)"

      case other =>
        q"""
          new sta.model.triggers.functions.ModelFunction[$tpe] {
            def apply(m: $tpe) = $other(m)
          }
         """
    }
  }
}
