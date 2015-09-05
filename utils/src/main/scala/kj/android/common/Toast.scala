package kj.android.common

import android.content.Context
import android.os.Handler
import android.widget.{Toast => AndroidToast}

object Toast {
  private[this] val handler = new Handler()

  sealed abstract class Length(val value: Int)
  case object Short extends Length(AndroidToast.LENGTH_SHORT)
  case object Long extends Length(AndroidToast.LENGTH_LONG)

  def apply(txt: String, length: Length = Short)(implicit ctx: Context): Unit = synchronized {
    handler.post(new Runnable {
      def run(): Unit = AndroidToast.makeText(ctx.getApplicationContext, txt, length.value).show()
    })
  }
}
