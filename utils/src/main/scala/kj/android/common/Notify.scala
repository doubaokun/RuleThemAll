package kj.android.common

import android.app.{NotificationManager, Notification}
import android.content.Context

object Notify {
  def apply(txt: String, tag: Option[String] = None)(implicit ctx: Context, appInfo: AppInfo) = {
    val n = new Notification.Builder(ctx)
      .setContentTitle(appInfo.name)
      .setSmallIcon(appInfo.smallIcon)
      .setContentText(txt)
      .setAutoCancel(true)
      .build()
    val m = ctx.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]
    m.notify(tag.orNull, 0, n)
  }
}
