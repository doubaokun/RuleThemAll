package sta.common

import android.content.Context
import android.os.Handler
import android.widget.{Toast => AndroidToast}

object Toast {
  private[this] val handler = new Handler()

  sealed abstract class Length(val value: Int)
  case object Short extends Length(AndroidToast.LENGTH_SHORT)
  case object Long extends Length(AndroidToast.LENGTH_LONG)

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.DefaultArguments"))
  def apply(txt: String, length: Length = Short)(implicit ctx: Context): Unit = synchronized {
    handler.post(new Runnable {
      def run(): Unit = AndroidToast.makeText(ctx.getApplicationContext, txt, length.value).show()
    })
  }
}
