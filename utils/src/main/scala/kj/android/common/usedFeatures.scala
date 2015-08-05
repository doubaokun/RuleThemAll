package kj.android.common

import scala.language.experimental.macros

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

class feature(features: String*) extends StaticAnnotation

class category(category: String) extends StaticAnnotation

class tags(tags: String*) extends StaticAnnotation

trait UsedFeatures[T] {
  def category: String

  def features: Set[String]
}

object UsedFeatures {
  implicit def materializeUsedFeatures[T]: UsedFeatures[T] = macro UsedFeaturesImpl.usedFeatures[T]
}

private class UsedFeaturesImpl(val c: blackbox.Context) extends MacrosHelpers {
  import c.universe._

  def usedFeatures[T: WeakTypeTag] = {
    val tpe = weakTypeOf[T]
    val annotations = allAnnotationsFor(c)(tpe)

    val category = annotations.map(_.tree).collectFirst {
      case q"""new $parent($arg)""" if parent.tpe =:= weakTypeOf[category] => arg
    }.getOrElse(c.abort(
      c.enclosingPosition,
      s"Cannot generate UsedFeatures for $tpe: no category defined."
    ))
    val features = annotations.map(_.tree).collectFirst {
      case q"""new $parent(..$args)""" if parent.tpe =:= weakTypeOf[feature] => args
    }.getOrElse(List.empty).foldLeft(q"Set.empty[String]") { case (acc, f) => q"$acc + $f" }

    q"""new kj.android.common.UsedFeatures[$tpe] {
      def category: String = $category

      def features: Set[String] = $features
    }"""
  }
}
