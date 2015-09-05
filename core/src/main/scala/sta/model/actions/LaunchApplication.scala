package sta.model.actions

import android.content.Context
import android.content.pm.PackageManager
import kj.android.logging.Logging
import scala.collection.convert.decorateAsScala._

case class LaunchApplication(appPackage: PackageManager => String) extends Action with Logging {
  def execute()(implicit ctx: Context): Unit = {
    val pm = ctx.getPackageManager
    ctx.startActivity(pm.getLaunchIntentForPackage(appPackage(pm)))
  }
}

object LaunchApplication {
  def apply(app: String) = new LaunchApplication(pm => {
    val matches = pm.getInstalledApplications(0).iterator().asScala.filter(appInfo =>
      pm.getApplicationLabel(appInfo) == app
    ).toList
    matches match {
      case Nil => sys.error(s"No application with name $app found")
      case info :: Nil => info.packageName
      case _ => sys.error(s"Ambiguous application name $app")
    }
  })

  def fromPackage(pkg: String) = new LaunchApplication(_ => pkg)
}
