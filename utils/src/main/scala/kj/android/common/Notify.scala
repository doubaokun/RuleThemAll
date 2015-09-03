package kj.android.common

import android.app.{PendingIntent, NotificationManager, Notification}
import android.content.Context
import kj.android.common.SystemServices._

object Notify {
  def apply(txt: String, tag: Option[String] = None)(implicit ctx: Context, appInfo: AppInfo) = {
    val m = notificationManager
    m.notify(tag.orNull, 0, build(txt))
  }

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
