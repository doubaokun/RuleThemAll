package sta.common

import scala.language.experimental.macros

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

class category(category: String) extends StaticAnnotation

class feature(features: String*) extends StaticAnnotation

class intent(intent: String*) extends StaticAnnotation

object UsedFeatures {
  implicit def materializeUsedFeatures[T]: UsedFeatures[T] = macro UsedFeaturesImpl.usedFeatures[T]
}

trait UsedFeatures[T] {
  def category: String

  def features: Set[String]

  def intents: Set[String]
}

private class UsedFeaturesImpl(val c: blackbox.Context) {
  import c.universe._

  /** Returns all annotations collected from `tpe` and its (grand)parents.
    */
  private def allParentAnnotationsFor(tpe: Type): List[Annotation] = {
    tpe.baseClasses.flatMap(_.annotations)
  }

  /** Returns all annotations collected from `tpe` and its direct subclasses..
    */
  private def allChildrenAnnotationsFor(tpe: Type): List[Annotation] = {
    if (tpe.typeSymbol.isClass) tpe.typeSymbol.annotations ++
      tpe.typeSymbol.asClass.knownDirectSubclasses.flatMap(_.annotations)(collection.breakOut)
    else Nil
  }

  def usedFeatures[T: WeakTypeTag] = {
    val tpe = weakTypeOf[T]
    val annotations = allParentAnnotationsFor(tpe)

    val category = annotations.map(_.tree).collectFirst {
      case q"""new $parent($arg)""" if parent.tpe =:= weakTypeOf[category] => arg
    }.getOrElse(c.abort(
      c.enclosingPosition,
      s"Cannot generate UsedFeatures for $tpe: no category defined."
    ))
    val features = annotations.map(_.tree).collectFirst {
      case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[feature] => args
    }.getOrElse(List.empty).foldLeft(q"Set.empty[String]") { case (acc, f) => q"$acc + $f" }
    val intents = allChildrenAnnotationsFor(tpe).map(_.tree).collect {
      case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[intent] => args
    }.flatten.foldLeft(q"Set.empty[String]") { case (acc, f) => q"$acc + $f" }

    q"""new sta.common.UsedFeatures[$tpe] {
      def category: String = $category

      def features: Set[String] = $features

      def intents: Set[String] = $intents
    }"""
  }
}
