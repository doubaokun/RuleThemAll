package sta.common

import android.app.{Notification, PendingIntent}
import android.content.Context

object Notify {
  import SystemServices._

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.DefaultArguments"))
  def apply(txt: String, tag: Option[String] = None)(implicit ctx: Context, appInfo: AppInfo) = {
    val m = notificationManager
    m.notify(tag.orNull, 0, build(txt))
  }

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.DefaultArguments"))
  def build(txt: String, onClick: Option[PendingIntent] = None)(implicit ctx: Context, appInfo: AppInfo) = {
    val builder = new Notification.Builder(ctx)
      .setContentTitle(appInfo.name)
      .setSmallIcon(appInfo.smallIcon)
      .setContentText(txt)
      .setAutoCancel(true)
    onClick.foreach(builder.setContentIntent)
    builder.build()
  }
}
