package kj.android.common

import scala.language.implicitConversions
import android.content.Intent
import android.os.{Parcelable, Bundle, Message, Messenger}
import kj.android.logging.LogTag
import scala.util.control.NonFatal

trait Common {
  @inline implicit def toRichBundle(data: Bundle): Common.RichBundle = new Common.RichBundle(data)

  @inline implicit def toRichIntent(intent: Intent): Common.RichIntent = new Common.RichIntent(intent)

  @inline implicit def toRichMessage(message: Message): Common.RichMessage = new Common.RichMessage(message)
}

object Common {
  class RichBundle protected[Common] (private val data: Bundle) extends AnyVal {
    @inline def put[T <: Serializable](key: String, value: T): Bundle = {
      data.putSerializable(key, value)
      data
    }

    @inline def putArray[T <: Parcelable](key: String, value: Array[T]): Bundle = {
      data.putParcelableArray(key, value.asInstanceOf[Array[Parcelable]])
      data
    }

    @inline def apply[T](key: String)(implicit logTag: LogTag): T = try {
      data.get(key).asInstanceOf[T]
    } catch {
      case NonFatal(th) =>
        android.util.Log.e(logTag.tag, s"Error during getting key $key from bundle", th)
        throw th
    }

    @inline def get[T](key: String)(implicit logTag: LogTag): Option[T] = try {
      Option(data.get(key)).asInstanceOf[Option[T]]
    } catch {
      case NonFatal(th) =>
        android.util.Log.e(logTag.tag, s"Error during getting key $key from bundle", th)
        None
    }
  }

  class RichIntent protected[Common] (private val intent: Intent) extends AnyVal {
    @inline def apply[T](key: String)(implicit logTag: LogTag): T = new RichBundle(intent.getExtras).apply(key)

    @inline def get[T](key: String)(implicit logTag: LogTag): Option[T] = new RichBundle(intent.getExtras).get(key)
  }

  class RichMessage protected[Common] (private val msg: Message) extends AnyVal {
    @inline def apply[T](key: String)(implicit logTag: LogTag): T = new RichBundle(msg.getData).apply(key)

    @inline def get[T](key: String)(implicit logTag: LogTag): Option[T] = new RichBundle(msg.getData).get(key)

    @inline def withData(data: Bundle): Message = {
      msg.setData(data)
      msg
    }

    @inline def withID(id: Int): Message = {
      msg.arg1 = id
      msg
    }
  }
}
