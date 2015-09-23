package sta.services

import android.content._
import android.content.pm.{PackageManager, ServiceInfo}
import android.os._
import android.util.SparseArray
import java.util.UUID
import java.util.concurrent.{CountDownLatch, TimeUnit}
import kj.android.common.Common
import kj.android.concurrent.ExecutionContext.Implicits._
import kj.android.concurrent.Task
import kj.android.logging.Logging
import scala.concurrent.duration._
import scala.util.control.NonFatal
import sta.common.Requirement
import sta.model.BaseModel
import sta.model.actions.Action
import sta.parser.{ActionParser, RulesParser, TriggerParser}
import sta.plugin.Plugin

trait PluginHandler extends RulesExecutor with Common with Logging { root =>
  import Plugin._

  private[this] val pluginThread = new HandlerThread(s"${logTag.tag}-Plugin")
  pluginThread.start()
  private[this] val pluginLooper = pluginThread.getLooper

  private[this] val pluginLock = new SparseArray[CountDownLatch]()
  private[this] val actionExecutionResult = new SparseArray[Option[Throwable]]()

  @inline private def lock[@specialized(Unit) T](id: Int, timeout: Duration)(body: => Unit)
    (onSuccess: => T, onError: Throwable => T, onTimeout: => T): T = try {
    val latch = new CountDownLatch(1)
    pluginLock.put(id, latch)
    body
    val succeeded = latch.await(timeout.length, timeout.unit)
    pluginLock.remove(id)
    if (succeeded) onSuccess else onTimeout
  } catch {
    case NonFatal(th) => onError(th)
  }

  private class PluginConnection extends ServiceConnection {
    private[this] var rawMessenger: Messenger = _
    private[this] var rawName: ComponentName = _
    private[this] var rawLatch = new CountDownLatch(1)

    def connectedTo: ComponentName = rawName

    def latch: CountDownLatch = rawLatch

    def executeAction(action: Action): Option[Throwable] = {
      val id = rawName.hashCode()

      val ok = rawLatch.await(2, TimeUnit.MINUTES)
      lazy val onTimeout = Some(new RuntimeException(s"Timeout during executing ${action.name} in $rawName"))
      val result = lock(id, 30.seconds) {
        rawMessenger.send(Message.obtain(null, EXECUTE_ACTION, id, 0)
          .withData(new Bundle().put(ACTION, action)))
      }({
        val result = actionExecutionResult.get(id, None)
        actionExecutionResult.remove(id)
        result
      }, Some(_), onTimeout)

      log.info(s"Action ${action.name} executed by plugin: $rawName")
      if (ok) result else onTimeout
    }

    def onServiceConnected(name: ComponentName, service: IBinder): Unit = {
      rawMessenger = new Messenger(service)
      rawName = name
      rawLatch.countDown()
    }

    def onServiceDisconnected(name: ComponentName): Unit = {
      rawMessenger = null
      rawName = null
      rawLatch = new CountDownLatch(1)
    }
  }

  private[this] val plugins = new SparseArray[PluginConnection]()
  private[this] val pluginActionParsers = new SparseArray[Class[_]]()
  private[this] val pluginTriggerParsers = new SparseArray[String]()

  private[this] val pluginHandler = new Handler(pluginLooper) {
    override def handleMessage(msg: Message): Unit = try {
      msg.what match {
        case REGISTER =>
          val plugin = msg.obj.asInstanceOf[ComponentName]
          log.info(s"Registering plugin: $plugin")

          val id = plugin.hashCode()
          val token = msg[UUID](TOKEN)
          val conn = new PluginConnection
          val launchIntent = new Intent(token.toString).setComponent(plugin)
          bindService(launchIntent, conn, Context.BIND_IMPORTANT)
          conn.latch.await()

          msg.get[ActionParser[Action]](ACTION_PARSER).foreach { p =>
            registerActionExecutor(p.actionClass, conn.executeAction)
            pluginActionParsers.put(id, p.actionClass)
            RulesParser.addActionParser(p)
          }
          msg.get[TriggerParser[BaseModel]](TRIGGER_PARSER).foreach { p =>
            pluginTriggerParsers.put(id, p.Prefix)
            RulesParser.addTriggerParser(p)
          }
          plugins.put(id, conn)

          pluginLock.get(id).countDown()
        case DEREGISTER =>
          val id = msg.arg1
          log.info(s"Deregistering plugin: ${plugins.get(id)}")

          pluginLock.get(id).countDown()
        case EXECUTE_ACTION =>
          val id = msg.arg1
          log.info(s"Processing execution of action from plugin: ${plugins.get(id).connectedTo}")

          actionExecutionResult.put(id, msg.get[Throwable](ACTION_EXCEPTION))
          pluginLock.get(id).countDown()
        case UPDATE_STATE =>
          val id = msg.arg1
          log.info(s"Updating state with model received by plugin: ${plugins.get(id).connectedTo}")

          msg.get[BaseModel](MODEL).foreach(m => Task.runWithWakeLock {
            updateState(m)
          })
        case RESET_TIMERS =>
          val id = msg.arg1
          log.info(s"Resetting timers requested by plugin: ${plugins.get(id).connectedTo}")

          msg.get[Array[Requirement]](FOR).foreach(reqs => resetTimers(reqs.toSet))
        case _ => log.warn(s"Unknown $msg", new RuntimeException)
      }
    } catch {
      case NonFatal(th) => log.error(s"Exception has occurred during handling message: $msg", th)
    }
  }
  private[this] val sender = new Messenger(pluginHandler)

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

  private def bindToService(service: ServiceInfo): Unit = {
    val name = new ComponentName(service.packageName, service.name)
    val id = name.hashCode()
    val launchIntent = new Intent().setComponent(name)

    lock(id, 20.seconds) {
      startService(launchIntent.putExtra(EXECUTOR, this.getClass).putExtra(REPLY_TO, sender))
    }({
      log.info(s"Plugin ${plugins.get(id).connectedTo} registered")
    }, { th =>
      log.error(s"Exception during registering plugin: $name", th)
      plugins.remove(id)
    }, {
      log.warn(s"Failed to register plugin: $name}")
      plugins.remove(id)
    })
  }

  private def bind(intent: Intent): Unit = {
    val pkg = intent.getData.getEncodedSchemeSpecificPart
    getPackageManager.getPackageInfo(pkg, PackageManager.GET_SERVICES).services.foreach(bindToService)
  }

  private def unbind(intent: Intent): Unit = {
    val pkg = intent.getData.getEncodedSchemeSpecificPart
    getPackageManager.getPackageInfo(pkg, PackageManager.GET_SERVICES).services.foreach { service =>
      val name = new ComponentName(service.packageName, service.name)
      val id = name.hashCode()
      val conn = plugins.get(id)

      def clean(): Unit = {
        plugins.remove(id)
        Option(pluginActionParsers.get(id)).foreach { clazz =>
          removeActionExecutor(clazz)
          RulesParser.removeActionParser(clazz)
        }
        pluginActionParsers.remove(id)
        Option(pluginTriggerParsers.get(id)).foreach(RulesParser.removeTriggerParser)
        pluginTriggerParsers.remove(id)
      }

      lock(id, 20.seconds) {
        unbindService(conn)
      }({
        log.info(s"Plugin $name deregistered")
        clean()
      }, { th =>
        log.error(s"Exception during deregistering plugin: $name", th)
        clean()
      },{
        log.warn(s"Forcing removal of unresponsive plugin: $name")
        clean()
      })
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

    pluginLooper.quit()
  }
}
