package rta.service

import android.content.{BroadcastReceiver, Context, Intent}
import rta.logging.Logging

class BootReceiver extends BroadcastReceiver with Logging {
  def onReceive(context: Context, intent: Intent): Unit = intent.getAction match {
    case Intent.ACTION_BOOT_COMPLETED =>
      val clazz = classOf[RulesService]
      log.info(s"Starting ${clazz.getSimpleName}")
      context.startService(new Intent(context, clazz))
    case other =>
      log.warn(s"Unknown message received: $other")
  }
}
