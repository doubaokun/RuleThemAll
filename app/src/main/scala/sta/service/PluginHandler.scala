package sta.service

import android.content._
import android.content.pm.{PackageManager, ServiceInfo}
import android.os._
import android.util.SparseArray
import java.util
import java.util.concurrent.locks.ReentrantLock
import scala.util.control.NonFatal
import sta.common.{Common, Requirement}
import sta.logging.Logging
import sta.model.BaseModel
import sta.model.actions.Action
import sta.parser.{ActionParser, RulesParser, TriggerParser}
import sta.plugin.{IPlugin, OnNewModel, OnResetTimers, Plugin, RemoteObject}

trait PluginHandler extends RulesExecutor with Common with Logging { root =>
  private object OnNewModelImpl extends OnNewModel.Stub {
    def onNewModel(model: RemoteObject): Unit = try {
      log.info("Handling external updateState request")

      updateState(model.as[BaseModel])
    } catch {
      case NonFatal(th) => log.error("Error whilst updating state", th)
    }
  }

  private object OnResetTimersImpl extends OnResetTimers.Stub {
    def onResetTimers(list: util.List[RemoteObject]): Unit = try {
      log.info("Handling external resetTimers request")

      import scala.collection.convert.decorateAsScala._
      resetTimers(list.asScala.map(_.as[Requirement])(collection.breakOut))
    } catch {
      case NonFatal(th) => log.error("Error whilst resetting timers", th)
    }
  }

  private class PluginConnection extends ServiceConnection {
    private[this] var plugin: IPlugin = _

    def executeAction(action: Action): Option[Throwable] = try {
      plugin.executeAction(new RemoteObject(action)).as[Option[Throwable]]
    } catch {
      case NonFatal(th) => Some(th)
    }

    def onServiceConnected(name: ComponentName, service: IBinder): Unit = {
      val id = name.hashCode()

      plugin = IPlugin.Stub.asInterface(service)
      plugin.register(OnNewModelImpl, OnResetTimersImpl)

      inLock {
        plugin.actionParser().as[Option[ActionParser[Action]]].foreach { p =>
          registerActionExecutor(p.actionClass, executeAction)
          pluginActionParsers.put(id, p.actionClass)
          RulesParser.addActionParser(p)
        }
        plugin.triggerParser().as[Option[TriggerParser[BaseModel]]].foreach { p =>
          pluginTriggerParsers.put(id, p.Prefix)
          RulesParser.addTriggerParser(p)
        }

        plugins.put(id, this)
      }
    }

    def onServiceDisconnected(name: ComponentName): Unit = {
      val id = name.hashCode()

      plugin = null

      inLock {
        plugins.remove(id)
      }
    }
  }

  private[this] val pluginLock = new ReentrantLock()

  private[this] val plugins = new SparseArray[PluginConnection]()
  private[this] val pluginActionParsers = new SparseArray[Class[_]]()
  private[this] val pluginTriggerParsers = new SparseArray[String]()

  private[this] val pluginUpdater = new BroadcastReceiver {
    def onReceive(context: Context, intent: Intent): Unit = try {
      intent.getAction match {
        case Intent.ACTION_PACKAGE_ADDED =>
          bind(intent)
        case Intent.ACTION_PACKAGE_REPLACED =>
          unbind(intent)
          bind(intent)
        case Intent.ACTION_PACKAGE_REMOVED | Intent.ACTION_PACKAGE_FULLY_REMOVED =>
          unbind(intent)
        case _ => log.warn(s"Unknown $intent", new RuntimeException)
      }
    } catch {
      case NonFatal(th) => log.error(s"Exception has occurred during handling intent: $intent", th)
    }
  }

  @inline private def inLock(body: => Unit) = {
    pluginLock.lock()
    try {
      body
    } finally {
      pluginLock.unlock()
    }
  }

  @inline private def bindToService(service: ServiceInfo): Unit = {
    val name = new ComponentName(service.packageName, service.name)
    log.info(s"Registering plugin $name")

    bindService(new Intent().setComponent(name), new PluginConnection,
      Context.BIND_AUTO_CREATE | Context.BIND_IMPORTANT)
  }

  private def bind(intent: Intent): Unit = {
    val pkg = intent.getData.getEncodedSchemeSpecificPart
    getPackageManager.getPackageInfo(pkg, PackageManager.GET_SERVICES).services.foreach(bindToService)
  }

  private def unbind(intent: Intent): Unit = {
    val pkg = intent.getData.getEncodedSchemeSpecificPart
    getPackageManager.getPackageInfo(pkg, PackageManager.GET_SERVICES).services.foreach { service =>
      val name = new ComponentName(service.packageName, service.name)
      log.info(s"Deregistering plugin $name")

      val id = name.hashCode()
      inLock {
        Option(pluginActionParsers.get(id)).foreach { clazz =>
          removeActionExecutor(clazz)
          RulesParser.removeActionParser(clazz)
        }
        pluginActionParsers.remove(id)
        Option(pluginTriggerParsers.get(id)).foreach(RulesParser.removeTriggerParser)
        pluginTriggerParsers.remove(id)

        unbindService(plugins.get(id))
        plugins.remove(id)
      }
    }
  }

  private def init(): Unit = {
    import scala.collection.convert.decorateAsScala._

    getPackageManager.queryIntentServices(new Intent().addCategory(Plugin.CATEGORY), 0)
      .asScala.foreach(info => bindToService(info.serviceInfo))
  }
  
  override def onCreate(): Unit = {
    super.onCreate()

    init()
    val filter = new IntentFilter()
    filter.addAction(Intent.ACTION_PACKAGE_ADDED)
    filter.addAction(Intent.ACTION_PACKAGE_REPLACED)
    filter.addAction(Intent.ACTION_PACKAGE_REMOVED)
    filter.addAction(Intent.ACTION_PACKAGE_FULLY_REMOVED)
    filter.addDataScheme("package")
    filter.addCategory(Plugin.CATEGORY)
    this.registerReceiver(pluginUpdater, filter, null, new Handler(mainLooper))
  }

  override def onDestroy(): Unit = {
    super.onDestroy()

    try {
      unregisterReceiver(pluginUpdater)
    } catch {
      case ex: IllegalArgumentException =>
    }
  }
}