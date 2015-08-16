package sta.services

import scala.language.experimental.macros
import scala.language.{dynamics, existentials, higherKinds}

import kj.android.common.feature
import scala.annotation.StaticAnnotation
import scala.concurrent.duration.Duration
import scala.reflect.macros.blackbox
import sta.model.Model

/**
  * Intents that should not be registered with receiver.
  * Instead, their state should be requested every specified interval.
  *
  * @param seq sequence of intent -> duration
  */
class manual(seq: (String, Duration)*) extends StaticAnnotation

object ServiceMacros {
  type SF = ServiceFragment[Model]

  case class RichService(actual: SF, manual: Option[Seq[(String, Duration)]], features: Seq[String])

  def collect: Seq[RichService] = macro ServiceMacrosImpl.collect
}

private class ServiceMacrosImpl(val c: blackbox.Context) {

  import c.universe._

  def collect = {
    val tpe = weakTypeOf[ServiceMacros.SF].dealias.typeConstructor
    val services = for {
      decl <- c.mirror.staticPackage("sta.services").typeSignature.decls
      inherited <- decl.typeSignature.baseClasses if
        !(inherited.typeSignature =:= decl.typeSignature) &&
          inherited.asType.toTypeConstructor =:= tpe
    } yield {
      val manual = decl.annotations.map(_.tree).collectFirst {
        case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[manual] => args
      }
      // FIXME partially overlaps with UsedFeatures
      val usesFeatures = decl.annotations.map(_.tree).collectFirst {
        case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[feature] => args
      }.getOrElse(List.empty).foldLeft(q"List.empty[String]") { case (acc, f) => q"$f :: $acc" }

      q"""
        sta.services.ServiceMacros.RichService(
          actual = (new ${decl.asType.toType}).asInstanceOf[sta.services.ServiceFragment[sta.model.Model]],
          manual = $manual,
          features = $usesFeatures
        )
      """
    }

    q"${services.toList}"
  }
}
