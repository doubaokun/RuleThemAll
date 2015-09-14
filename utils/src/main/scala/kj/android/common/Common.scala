package kj.android.common

import scala.language.implicitConversions
import android.content.Intent
import kj.android.logging.{LogTag, Logger}
import scala.util.control.NonFatal

trait Common {
  implicit def toRichIntent(intent: Intent): Common.RichIntent = new Common.RichIntent(intent)
}

object Common {
  class RichIntent protected[Common] (private val intent: Intent) extends AnyVal {
    def extra[T] = new ExpandExtra[T](intent)
  }

  class ExpandExtra[T] protected[Common] (private val intent: Intent) extends AnyVal {
    def apply(key: String)(implicit logTag: LogTag): T = try {
      Option(intent.getExtras.get(key)).getOrElse(throw new NullPointerException).asInstanceOf[T]
    } catch {
      case NonFatal(th) =>
        android.util.Log.e(logTag.tag, s"Error during getting key $key from intent", th)
        throw th
    }
    
    def get(key: String)(implicit logTag: LogTag): Option[T] = try {
      Option(intent.getSerializableExtra(key)).asInstanceOf[Option[T]]
    } catch {
      case NonFatal(th) =>
        android.util.Log.e(logTag.tag, s"Error during getting key $key from intent", th)
        None
    }
  }
}
