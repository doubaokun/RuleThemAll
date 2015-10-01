package sta.service

import scala.language.experimental.macros
import scala.language.{dynamics, existentials, higherKinds}
import scala.annotation.StaticAnnotation
import scala.concurrent.duration.Duration
import scala.reflect.macros.blackbox
import sta.common.{Requirement, Uses}
import sta.model.BaseModel

/** Marks intents that shouldn't be registered by a broadcast receiver,
  * but queried on a regular interval.
  */
class manual(seq: (String, Duration)*) extends StaticAnnotation

object ServiceMacros {
  type SF = ServiceFragment[BaseModel]

  case class RichService(actual: SF, manual: Option[Seq[(String, Duration)]], uses: Uses[BaseModel])

  def collect(enclosing: RulesExecutor): Seq[RichService] = macro ServiceMacrosImpl.collect
}

private class ServiceMacrosImpl(val c: blackbox.Context) {

  import c.universe._

  private def RichService = c.typeOf[ServiceMacros.RichService]

  private def Uses(tpe: Type) = appliedType(typeOf[Uses[_]].typeConstructor, tpe)

  private def Uses = c.typeOf[Uses[BaseModel]]

  private def ServiceFragment = c.typeOf[ServiceFragment[BaseModel]]

  def collect(enclosing: c.Expr[RulesExecutor]) = {
    val enclosingTpe = weakTypeOf[RulesExecutor]
    val services = for {
      decl <- c.mirror.staticPackage("sta.service").typeSignature.decls
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
          ctors.head.asMethod.paramLists match {
            case List(Nil) => q"new $tpe"
            case List(arg :: Nil) if enclosingTpe <:< arg.typeSignature => q"new $tpe($enclosing)"
            case List(Nil, arg :: Nil) if enclosingTpe <:< arg.typeSignature => q"new $tpe()($enclosing)"
            case args =>
              val ctor = args.map(_.map(_.typeSignature).mkString("(", ", ", ")")).mkString
              c.abort(c.enclosingPosition,
                s"""Cannot create service fragment using constructor with args: $ctor""")
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
