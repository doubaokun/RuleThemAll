package sta.services

import scala.language.experimental.macros
import scala.language.{dynamics, existentials, higherKinds}
import scala.annotation.StaticAnnotation
import scala.concurrent.duration.Duration
import scala.reflect.macros.blackbox
import sta.common.UsedFeatures
import sta.model.Model

/** Marks intents that shouldn't be registered by a broadcast receiver,
  * but queried on a regular interval.
  */
class manual(seq: (String, Duration)*) extends StaticAnnotation

object ServiceMacros {
  type SF = ServiceFragment[Model]

  case class RichService(actual: SF, manual: Option[Seq[(String, Duration)]], features: UsedFeatures[Model])

  def collect: Seq[RichService] = macro ServiceMacrosImpl.collect
}

private class ServiceMacrosImpl(val c: blackbox.Context) {

  import c.universe._

  private def RichService = c.typeOf[ServiceMacros.RichService]

  private def UsedFeatures(tpe: Type) = appliedType(typeOf[UsedFeatures[_]].typeConstructor, tpe)

  private def UsedFeatures = c.typeOf[UsedFeatures[Model]]

  private def ServiceFragment = c.typeOf[ServiceFragment[Model]]

  def collect = {
    val services = for {
      decl <- c.mirror.staticPackage("sta.services").typeSignature.decls
      inherited <- decl.typeSignature.baseClasses if
      !(inherited.typeSignature =:= decl.typeSignature) &&
        inherited.asType.toTypeConstructor =:= ServiceFragment.typeConstructor
    } yield {
        val manual = decl.annotations.map(_.tree).collectFirst {
          case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[manual] => args
        }
        q"""
        new $RichService(
          actual = (new ${decl.asType.toType}).asInstanceOf[$ServiceFragment],
          manual = $manual,
          features = implicitly[${UsedFeatures(decl.typeSignature.baseType(inherited).typeArgs.head)}].asInstanceOf[$UsedFeatures]
        )
      """
      }

    q"${services.toList}"
  }
}
