package kj.android.logging

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
  * Note, that calling LogTag constructor directly should be avoided!
  * You should use apply method in companion object instead
  */
class LogTag(val tag: String) extends AnyVal {
  override def toString: String = tag
}

object LogTag {
  def apply(tag: String): LogTag = macro safeApply

  @compileTimeOnly("safeApply cannot be called outside companion apply method")
  def safeApply(c: blackbox.Context)(tag: c.Expr[String]) = {
    import c.universe._
    tag.tree match {
      case q"${ t: String }" if t.length > 23 ⇒ q"new LogTag(${t.substring(0, 23)})"
      case q"${ t: String }"                  ⇒ q"new LogTag($t)"
      case _                                  ⇒ c.abort(c.enclosingPosition, "Log tag must be a literal")
    }
  }
}