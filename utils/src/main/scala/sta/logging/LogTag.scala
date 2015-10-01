package sta.logging

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** LogTag for the [[android.util.Log)]].
  *
  * Note that you should avoid calling constructor directly and use companion `apply` method instead.
  */
class LogTag(val tag: String) extends AnyVal {
  override def toString: String = tag
}

object LogTag {
  def apply(tag: String): LogTag = macro LogTagImpl.makeInstance
}

private class LogTagImpl(val c: blackbox.Context) {
  import c.universe._

  def makeInstance(tag: c.Expr[String]) = {
    tag.tree match {
      case q"${t: String}" if t.length > 23 => q"new ${typeOf[LogTag]}(${t.substring(0, 23)})"
      case q"${t: String}" => q"new ${typeOf[LogTag]}($t)"
      case _ => q"new ${typeOf[LogTag]}($tag.substring(0, scala.math.min($tag.length, 23)))"
    }
  }
}
