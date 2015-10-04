package sta.common

import android.app.{AlarmManager, NotificationManager}
import android.content.Context
import android.media.AudioManager
import android.net.ConnectivityManager
import android.net.wifi.WifiManager
import android.os.PowerManager
import android.telephony.TelephonyManager


object SystemServices {
  @inline def alarmManager(implicit ctx: Context) =
    ctx.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]

  @inline def audioManger(implicit ctx: Context) =
    ctx.getSystemService(Context.AUDIO_SERVICE).asInstanceOf[AudioManager]

  @inline def connectivityManager(implicit ctx: Context) =
    ctx.getSystemService(Context.CONNECTIVITY_SERVICE).asInstanceOf[ConnectivityManager]

  @inline def notificationManager(implicit ctx: Context) =
    ctx.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]

  @inline def powerManager(implicit ctx: Context) =
    ctx.getSystemService(Context.POWER_SERVICE).asInstanceOf[PowerManager]

  @inline def telephonyManager(implicit ctx: Context)=
    ctx.getSystemService(Context.TELEPHONY_SERVICE).asInstanceOf[TelephonyManager]

  @inline def wifiManager(implicit ctx: Context) =
    ctx.getSystemService(Context.WIFI_SERVICE).asInstanceOf[WifiManager]
}
