package sta.model.actions

import android.content.Context
import android.content.pm.PackageManager
import kj.android.logging.Logging
import scala.collection.convert.decorateAsScala._

sealed abstract class LaunchApplication extends Action with Logging { this: Product =>
  def getAppPackage(pm: PackageManager): String

  def execute()(implicit ctx: Context): Unit = {
    val pm = ctx.getPackageManager
    ctx.startActivity(pm.getLaunchIntentForPackage(getAppPackage(pm)))
  }
}

object LaunchApplication {
  case class FromPackage(pkg: String) extends LaunchApplication {
    def getAppPackage(pm: PackageManager): String = pkg
  }

  case class UsingAppName(app: String) extends LaunchApplication {
    def getAppPackage(pm: PackageManager): String = {
      val matches = pm.getInstalledApplications(0).iterator().asScala.filter(appInfo =>
        pm.getApplicationLabel(appInfo) == app
      ).toList
      matches match {
        case Nil => sys.error(s"No application with name $app found")
        case info :: Nil => info.packageName
        case _ => sys.error(s"Ambiguous application name $app")
      }
    }
  }
}