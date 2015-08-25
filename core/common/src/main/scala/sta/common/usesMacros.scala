package sta.common

import scala.language.experimental.macros
import android.content.Intent
import android.net.Uri
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

class category(name: String) extends StaticAnnotation

class feature(name: String*) extends StaticAnnotation

class action(name: String) extends StaticAnnotation

class data(uri: Uri) extends StaticAnnotation

object Uses {
  implicit def materializeUses[T]: Uses[T] = macro UsesImpl.usesOf[T]

  def categoryOf[T]: String = macro UsesImpl.categoryOf[T]

  def dataOf[T]: Uri = macro UsesImpl.dataOf[T]
}

trait Uses[T] {
  def features: Set[String]

  def intents: Set[Intent]
}

private class UsesImpl(val c: blackbox.Context) {
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
        case (Some(at), Some(ut)) => Some(q"new $Intent($at, $ut)")
        case (Some(at), None) => Some(q"new $Intent($at)")
        case (None, Some(ut)) => c.abort(c.enclosingPosition,
          "`@data` annotation should be used with the `@action` annotation.")
        case (None, None) => None
      }
    }
  }

  def categoryOf[T: WeakTypeTag] = {
    val tpe = weakTypeOf[T]

    allParentAnnotationsFor(tpe).map(_.tree).collectFirst {
      case q"""new $annotation($arg)""" if annotation.tpe =:= typeOf[category] => arg
    }.getOrElse(c.abort(c.enclosingPosition, s"Cannot find category for $tpe."))
  }

  def dataOf[T: WeakTypeTag] = {
    val tpe = weakTypeOf[T]

    tpe.typeSymbol.annotations.map(_.tree).collectFirst {
      case q"""new $annotation($arg)""" if annotation.tpe =:= typeOf[data] => arg
    }.getOrElse(c.abort(c.enclosingPosition, s"Cannot find data uri for $tpe."))
  }

  def usesOf[T: WeakTypeTag] = {
    val tpe = weakTypeOf[T]

    val features = allChildrenAnnotationsFor(tpe).flatten.map(_.tree).collect {
      case q"""new $annotation(..$args)""" if annotation.tpe =:= typeOf[feature] => args
    }.flatten.foldLeft(q"Set.empty[String]") { case (acc, f) => q"$acc + $f" }
    val intents = allChildrenAnnotationsFor(tpe).collect {
      case IntentExtractor(tree) => tree
    }.foldLeft(q"Set.empty[$Intent]") { case (acc, f) => q"$acc + $f" }

    q"""new ${appliedType(typeOf[Uses[_]].typeConstructor, tpe)} {
      def features: Set[String] = $features

      def intents: Set[$Intent] = $intents
    }"""
  }
}
