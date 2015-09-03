package kj.android.common

import android.app.{NotificationManager, AlarmManager}
import android.content.Context
import android.media.AudioManager
import android.os.PowerManager

object SystemServices {
  @inline def alarmManager(implicit ctx: Context) =
    ctx.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]

  @inline def audioManger(implicit ctx: Context) =
    ctx.getSystemService(Context.AUDIO_SERVICE).asInstanceOf[AudioManager]

  @inline def notificationManager(implicit ctx: Context) =
    ctx.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]

  @inline def powerManager(implicit ctx: Context) =
    ctx.getSystemService(Context.POWER_SERVICE).asInstanceOf[PowerManager]
}
