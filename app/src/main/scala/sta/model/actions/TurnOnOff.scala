package sta.model.actions

import android.bluetooth.BluetoothAdapter
import android.content.{Context, Intent}
import android.net._
import android.os.Build
import android.provider.Settings
import android.telephony.TelephonyManager
import sta.common.SystemServices._
import sta.common.{Reflect, Root}

sealed abstract class TurnOnOff extends Action { this: Product =>
  def enable: Boolean
}

object TurnOnOff {
  final case class AirplaneMode(enable: Boolean) extends TurnOnOff {
    import Root._

    def execute()(implicit ctx: Context): Unit = {
      run(
        putSettings("global", Settings.Global.AIRPLANE_MODE_ON, enable.toInt),
        sendBroadcast(Intent.ACTION_AIRPLANE_MODE_CHANGED, "state" -> true)
      )
    }
  }

  final case class Bluetooth(enable: Boolean) extends TurnOnOff {
    def execute()(implicit ctx: Context): Unit = enable match {
      case true => BluetoothAdapter.getDefaultAdapter.enable()
      case false => BluetoothAdapter.getDefaultAdapter.disable()
    }
  }

  final case class MobileNetwork(enable: Boolean) extends TurnOnOff {
    import Reflect._
    import Root._

    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Null"))
    def execute()(implicit ctx: Context): Unit = {
      if (Build.VERSION.SDK_INT > Build.VERSION_CODES.KITKAT) {
        classOf[TelephonyManager].reflect[Unit](telephonyManager).setDataEnabled(enable)
      } else {
        classOf[ConnectivityManager].reflect[Unit](connectivityManager).setMobileDataEnabled(enable)
      }
    }

    override def prepare()(implicit ctx: Context): Unit = {
      if (Build.VERSION.SDK_INT > Build.VERSION_CODES.KITKAT) {
        run(grantPermission(ctx, android.Manifest.permission.MODIFY_PHONE_STATE))
      }
    }
  }

  final case class WiFi(enable: Boolean) extends TurnOnOff {
    def execute()(implicit ctx: Context): Unit = wifiManager.setWifiEnabled(enable)
  }
}