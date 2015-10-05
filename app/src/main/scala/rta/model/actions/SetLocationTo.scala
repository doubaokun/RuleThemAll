package rta.model.actions

import android.content.Context
import android.os.UserHandle
import android.provider.Settings
import enumeratum.Enum
import rta.common.{Reflect, Root}
import rta.model.FromIntEntry

final case class SetLocationTo(mode: SetLocationTo.Mode) extends SetTo {
  import Reflect._
  import Root._

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Null"))
  def execute()(implicit ctx: Context): Unit = {
    classOf[Settings.Secure].reflect[Boolean].setLocationModeForUser(
      ctx.getContentResolver, mode.intValue, classOf[UserHandle].reflect[Int].myUserId())
  }

  override def prepare()(implicit ctx: Context): Unit = {
    run(grantPermission(ctx, android.Manifest.permission.WRITE_SECURE_SETTINGS))
  }
}

object SetLocationTo {
  sealed abstract class Mode extends FromIntEntry
  object Mode extends Enum[Mode] {
    lazy val values = findValues

    case object HighAccuracy extends Mode {
      def intValue: Int = Settings.Secure.LOCATION_MODE_HIGH_ACCURACY
    }
    case object Sensors extends Mode {
      def intValue: Int = Settings.Secure.LOCATION_MODE_SENSORS_ONLY
    }
    case object BatterySaving extends Mode {
      def intValue: Int = Settings.Secure.LOCATION_MODE_BATTERY_SAVING
    }
    case object None extends Mode {
      def intValue: Int = Settings.Secure.LOCATION_MODE_OFF
    }
  }
}
