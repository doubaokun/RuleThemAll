package sta.common

import scala.language.experimental.macros
import android.content.Intent
import android.net.Uri
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

class category(name: String) extends StaticAnnotation

class feature(name: String*) extends StaticAnnotation

class action(name: String) extends StaticAnnotation

class data(uri: String) extends StaticAnnotation

object UsedFeatures {
  implicit def materializeUsedFeatures[T]: UsedFeatures[T] = macro UsedFeaturesImpl.usedFeatures[T]
}

trait UsedFeatures[T] {
  def category: String

  def features: Set[String]

  def intents: Set[Intent]
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
  private def allChildrenAnnotationsFor(tpe: Type): List[List[Annotation]] = {
    if (tpe.typeSymbol.isClass) tpe.typeSymbol.annotations ::
      tpe.typeSymbol.asClass.knownDirectSubclasses.map(_.annotations).toList
    else Nil
  }

  private def Intent = c.typeOf[Intent]

  private def Uri = c.typeOf[Uri].companion

  private object IntentExtractor {
    def unapply(annotations: List[Annotation]): Option[Tree] = {
      val trees = annotations.map(_.tree)

      val action = trees.collectFirst {
        case q"""new $annotation($arg)""" if annotation.tpe =:= typeOf[action] => arg
      }
      val uri = trees.collectFirst {
        case q"""new $annotation($arg)""" if annotation.tpe =:= typeOf[data] => arg
      }
      (action, uri) match {
        case (Some(at), Some(ut)) => Some(q"new $Intent($at, $Uri.parse($ut))")
        case (Some(at), None) => Some(q"new $Intent($at)")
        case (None, Some(ut)) => c.abort(c.enclosingPosition,
          "`@data` annotation should be used with the `@action` annotation.")
        case (None, None) => None
      }
    }
  }

  def usedFeatures[T: WeakTypeTag] = {
    val tpe = weakTypeOf[T]

    val category = allParentAnnotationsFor(tpe).map(_.tree).collectFirst {
      case q"""new $annotation($arg)""" if annotation.tpe =:= typeOf[category] => arg
    }.getOrElse(c.abort(
      c.enclosingPosition,
      s"Cannot generate UsedFeatures for $tpe: no category defined."
    ))
    val features = allChildrenAnnotationsFor(tpe).flatten.map(_.tree).collect {
      case q"""new $annotation(..$args)""" if annotation.tpe =:= typeOf[feature] => args
    }.flatten.foldLeft(q"Set.empty[String]") { case (acc, f) => q"$acc + $f" }
    val intents = allChildrenAnnotationsFor(tpe).collect {
      case IntentExtractor(tree) => tree
    }.foldLeft(q"Set.empty[$Intent]") { case (acc, f) => q"$acc + $f" }

    q"""new ${appliedType(typeOf[UsedFeatures[_]].typeConstructor, tpe)} {
      def category: String = $category

      def features: Set[String] = $features

      def intents: Set[$Intent] = $intents
    }"""
  }
}
