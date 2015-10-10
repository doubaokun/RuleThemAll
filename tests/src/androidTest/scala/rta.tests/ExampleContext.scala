package rta.tests

import android.app.Service
import android.content._
import android.content.pm.{PackageInfo, PackageManager, ResolveInfo, ServiceInfo}
import android.os.Handler
import android.test.mock.MockPackageManager
import java.util
import scala.reflect.{ClassTag, classTag}

class ExampleContext[S <: Service: ClassTag](pkg: String, base: Context) extends ContextWrapper(base) {
  def clazz = classTag[S].runtimeClass

  val name = clazz.getName

  private[this] var bound = false
  private[this] var component: ComponentName = _
  private[this] var connection: ServiceConnection = _
  private[this] var rawService: S = _
  private[this] var bindIntent: Intent = _

  def service: S = rawService

  private def initService(): Unit = {
    rawService = clazz.newInstance().asInstanceOf[S]
    rawService.onCreate()
    val m = classOf[ContextWrapper].getDeclaredMethod("attachBaseContext", classOf[Context])
    m.setAccessible(true)
    m.invoke(rawService, this)
  }

  override def getPackageManager: PackageManager = new MockPackageManager {
    override def getPackageInfo(packageName: String, flags: Int): PackageInfo = packageName match {
      case `pkg` =>
        val pi = new PackageInfo
        pi.services = Array({
          val si = new ServiceInfo()
          si.packageName = pkg
          si.name = name
          si
        })
        pi
      case _ => super.getPackageInfo(packageName, flags)
    }

    override def queryIntentServices(intent: Intent, flags: Int): util.List[ResolveInfo] =
      new util.ArrayList[ResolveInfo]()
  }

  override def bindService(service: Intent, conn: ServiceConnection, flags: Int): Boolean = service match {
    case _ if Option(service.getComponent).exists(c => c.getPackageName == pkg && c.getClassName == name) =>
      if (rawService == null) initService()
      if (!bound) {
        bindIntent = service
        val binder = rawService.onBind(bindIntent)
        conn.onServiceConnected(bindIntent.getComponent, binder)
        component = bindIntent.getComponent
        connection = conn
        bound = true
      }
      bound
    case _ => super.bindService(service, conn, flags)
  }

  override def unbindService(conn: ServiceConnection): Unit = conn match {
    case _ if conn == connection && bindIntent != null =>
      rawService.onUnbind(bindIntent)
      rawService = null.asInstanceOf[S]
      bindIntent = null
      component = null
      connection = null
      bound = false
    case _ => super.unbindService(conn)
  }

  override def registerReceiver(receiver: BroadcastReceiver, filter: IntentFilter, broadcastPermission: String, scheduler: Handler): Intent = {
    super.registerReceiver(receiver, filter, broadcastPermission, scheduler)
  }

  override def startService(service: Intent): ComponentName = service match {
    case _ if Option(service.getComponent).exists(c => c.getPackageName == pkg && c.getClassName == name) =>
      if (rawService == null) initService()
      rawService.onStartCommand(service, 0, 0)
      service.getComponent
    case _ => super.startService(service)
  }
}
