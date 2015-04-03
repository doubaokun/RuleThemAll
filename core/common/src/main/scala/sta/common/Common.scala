package sta.common

import android.content.Intent
import kj.android.logging.{ LogTag, Logger }
import kj.android.common.Option

import scala.language.{ dynamics, implicitConversions }

trait Common {
  implicit def toRichIntent(intent: Intent): Common.RichIntent = new Common.RichIntent(intent)
}

object Common {
  class RichIntent protected[Common] (private val intent: Intent) extends AnyVal {
    def extra[T] = ExpandExtra[T](intent)
  }

  case class ExpandExtra[T] protected[Common] (private val intent: Intent) extends AnyVal with Dynamic {
    def selectDynamic(field: String)(implicit logTag: LogTag): Option[T] = try {
      Option(intent.getExtras.get(field).asInstanceOf[T])
    } catch {
      case th: Throwable ⇒
        Logger.error(s"Error during getting $field from intent", th)
        Option.empty[T]
    }
  }
}
