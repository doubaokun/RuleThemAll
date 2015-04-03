package sta.services

import kj.android.common.feature
import sta.model.Model

import scala.annotation.StaticAnnotation
import scala.concurrent.duration.Duration
import scala.language.experimental.macros
import scala.language.{ dynamics, higherKinds }
import scala.reflect.macros.blackbox

class genReactOn extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ServiceMacrosImpl.genReactOn
}

/**
  * Intents that should not be registered with receiver.
  * Instead, their state should be requested on specified interval.
  *
  * @param seq sequence of intent -> duration
  */
class manual(seq: (String, Duration)*) extends StaticAnnotation

object ServiceMacros {
  type SF = ServiceFragment[Model]

  case class RichResult(actual: SF, manual: Option[Seq[(String, Duration)]], features: Seq[String])

  def collect: Seq[RichResult] = macro ServiceMacrosImpl.collect
}

private class ServiceMacrosImpl(val c: blackbox.Context) {
  import c.universe._

  def genReactOn(annottees: Expr[Any]*) = {
    val outputs = annottees.map { tdef ⇒
      val q"$tmods class $tpname[..$tparams] extends ServiceFragment[..$ttparams] with ..$parents { $self => ..$tbody }" = tdef.tree.duplicate
      val intents = (for {
        te ← tbody
        q"$dmods def handle($darg: Intent): $dtpe = $dbody" ← te
        q"$expr match { case ..$cases }" ← dbody
        cq"$intent if $expr => $cbody" ← cases
      } yield intent).toSet
      val f = q"protected[sta] def reactOn: Set[String] = $intents"
      q"$tmods class $tpname[..$tparams] extends ServiceFragment[..$ttparams] with ..$parents { $self => ..${tbody :+ f} }"
    }

    q"{ ..$outputs }"
  }

  def collect = {
    val tpe = weakTypeOf[ServiceMacros.SF].dealias.typeConstructor
    val services = for {
      decl ← c.mirror.staticPackage("sta.services").typeSignature.decls
      inherited ← decl.typeSignature.baseClasses if !(inherited.typeSignature =:= decl.typeSignature) && inherited.asType.toTypeConstructor =:= tpe
    } yield {
      val manual = decl.annotations.map(_.tree).collectFirst {
        case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[manual] ⇒ args
      }
      val usesFeatures = decl.annotations.map(_.tree).collectFirst {
        case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[feature] ⇒ args
      }.getOrElse(List.empty).foldLeft(q"List.empty[String]") { case (acc, f) ⇒ q"$f :: $acc" }

      q"""
        sta.services.ServiceMacros.RichResult(
          actual = new ${decl.asType.toType} {},
          manual = $manual,
          features = $usesFeatures
        )
      """
    }

    q"${services.toList}"
  }
}
