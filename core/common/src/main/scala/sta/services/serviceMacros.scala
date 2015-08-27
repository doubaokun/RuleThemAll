package sta.services

import scala.language.experimental.macros
import scala.language.{dynamics, existentials, higherKinds}
import scala.annotation.StaticAnnotation
import scala.concurrent.duration.Duration
import scala.reflect.macros.blackbox
import sta.common.{Requirement, Uses}
import sta.model.Model

/** Marks intents that shouldn't be registered by a broadcast receiver,
  * but queried on a regular interval.
  */
class manual(seq: (String, Duration)*) extends StaticAnnotation

object ServiceMacros {
  type SF = ServiceFragment[Model]

  case class RichService(actual: SF, manual: Option[Seq[(String, Duration)]], uses: Uses[Model])

  def collect[T](enclosing: T): Seq[RichService] = macro ServiceMacrosImpl.collect[T]
}

private class ServiceMacrosImpl(val c: blackbox.Context) {

  import c.universe._

  private def RichService = c.typeOf[ServiceMacros.RichService]

  private def Uses(tpe: Type) = appliedType(typeOf[Uses[_]].typeConstructor, tpe)

  private def Uses = c.typeOf[Uses[Model]]

  private def ServiceFragment = c.typeOf[ServiceFragment[Model]]

  def collect[T: WeakTypeTag](enclosing: c.Expr[T]) = {
    val enclosingTpe = weakTypeOf[T]
    val services = for {
      decl <- c.mirror.staticPackage("sta.services").typeSignature.decls
      inherited <- decl.typeSignature.baseClasses if
      !(inherited.typeSignature =:= decl.typeSignature) &&
        inherited.asType.toTypeConstructor =:= ServiceFragment.typeConstructor
    } yield {
        val manual = decl.annotations.map(_.tree).collectFirst {
          case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[manual] => args
        }
        val makeInstance = {
          val tpe = decl.asType.toType
          val ctors = tpe.decl(termNames.CONSTRUCTOR).alternatives
          if (ctors.length != 1) c.abort(c.enclosingPosition,
            s"Cannot create service fragment with multiple constructors: ${decl.asType}")
          ctors.head.asMethod.paramLists.flatten match {
            case Nil => q"new $tpe"
            case arg :: Nil if enclosingTpe <:< arg.typeSignature => q"new $tpe($enclosing)"
            case args => c.abort(c.enclosingPosition,
              s"""Cannot create service fragment using constructor with args: ${args.map(_.typeSignature).mkString("(", ", ", ")")}""")
          }
        }
        q"""
        new $RichService(
          actual = $makeInstance.asInstanceOf[$ServiceFragment],
          manual = $manual,
          uses = implicitly[${Uses(decl.typeSignature.baseType(inherited).typeArgs.head)}].asInstanceOf[$Uses]
        )
      """
      }

    q"${services.toList}"
  }
}
