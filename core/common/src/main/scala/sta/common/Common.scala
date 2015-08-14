package sta.common

import scala.language.{ dynamics, implicitConversions }

import android.content.Intent
import kj.android.logging.{ LogTag, Logger }

trait Common {
  implicit def toRichIntent(intent: Intent): Common.RichIntent = new Common.RichIntent(intent)
}

object Common {

  class RichIntent protected[Common] (private val intent: Intent) extends AnyVal {
    def extra[T] = new ExpandExtra[T](intent)
  }

  class ExpandExtra[T] protected[Common] (private val intent: Intent) extends AnyVal {
    def get(key: String)(implicit logTag: LogTag): T = try {
      Option(intent.getExtras.get(key)).getOrElse(throw new NullPointerException).asInstanceOf[T]
    } catch {
      case th: Throwable =>
        Logger.error(s"Error during getting $key from intent", th)
        throw th
    }
  }

}
