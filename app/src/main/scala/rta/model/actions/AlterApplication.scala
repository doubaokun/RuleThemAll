package rta.model.actions

import android.content.Context
import android.content.pm.PackageManager
import rta.common.Root
import rta.logging.Logging
import scala.collection.convert.decorateAsScala._

sealed abstract class AlterApplication extends Action with Logging {
  def appOrPackage: String

  def resolver: AlterApplication.Resolver

  override def kind: ActionKind =
    classOf[AlterApplication] -> None // TODO differentiate
}

object AlterApplication {

  type Resolver = (String, PackageManager) => String

  object Resolver {
    lazy val fromPackage: Resolver = (pkg, _) => pkg

    lazy val fromAppName: Resolver = (app, pm) => {
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

  final case class Launch(appOrPackage: String, resolver: Resolver) extends AlterApplication {
    def execute()(implicit ctx: Context): Unit = {
      val pm = ctx.getPackageManager
      ctx.startActivity(pm.getLaunchIntentForPackage(resolver(appOrPackage, pm)))
    }
  }

  final case class Kill(appOrPackage: String, resolver: Resolver) extends AlterApplication {
    import Root._

    def execute()(implicit ctx: Context): Unit = {
      val pkg = resolver(appOrPackage, ctx.getPackageManager)
      run(s"am force-stop $pkg")
    }

    override def prepare()(implicit ctx: Context): Unit =
      run(grantPermission(ctx, "android.permission.FORCE_STOP_PACKAGES"))
  }
}
