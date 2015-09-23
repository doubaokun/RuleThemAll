package sta.plugin

import android.app.Service
import android.content.{ComponentName, Intent}
import android.os._
import java.util.UUID
import kj.android.common.Common
import kj.android.logging.Logging
import scala.util.control.NonFatal
import sta.common.Requirement
import sta.model.BaseModel
import sta.model.actions.Action
import sta.parser.{ActionParser, TriggerParser}
import sta.services.RulesExecutor

class Plugin[A <: Action, M <: BaseModel] extends Service with Common with Logging { ctx =>
  import Plugin._

  @inline private def remote(block: => Unit) = try {
    block
  } catch {
    case th: DeadObjectException =>
      log.error("Host app has died", th)
      stopSelf()
  }
  
  private[this] val handlerThread = new HandlerThread(logTag.tag)
  handlerThread.start()

  private[this] val looper = handlerThread.getLooper

  private[this] val token: UUID = UUID.randomUUID()

  private[this] var replyTo: Messenger = _

  private[this] lazy val id = new ComponentName(this, this.getClass).hashCode()

  private[this] val handler = new Messenger(new Handler(looper) {
    override def handleMessage(msg: Message): Unit = remote {
      msg.what match {
        case EXECUTE_ACTION =>
          log.info("Executing action")
          val action = msg.getData.get(ACTION).asInstanceOf[A]
          val data = new Bundle()
          try {
            action.execute()(ctx)
          } catch {
            case NonFatal(th) => data.putSerializable(ACTION_EXCEPTION, th)
          }
          replyTo.send(Message.obtain(null, EXECUTE_ACTION, msg.arg1, msg.arg2, msg.obj).withData(data))
        case _ => log.warn(s"Unknown $msg", new RuntimeException)
      }
    }
  })

  private def register(): Unit = {
    log.info(s"Registering plugin")
    val name = new ComponentName(this, this.getClass)
    val data = new Bundle()
    actionParser.foreach(data.putSerializable(ACTION_PARSER, _))
    triggerParser.foreach(data.putSerializable(TRIGGER_PARSER, _))
    data.putSerializable(TOKEN, token)
    replyTo.send(Message.obtain(null, REGISTER, name).withData(data))
  }

  def actionParser: Option[ActionParser[A]] = None

  def triggerParser: Option[TriggerParser[M]] = None

  final def update(model: M): Unit = remote {
    log.info("Updating state")
    replyTo.send(Message.obtain(null, UPDATE_STATE)
      .withData(new Bundle().put(MODEL, model)).withID(id))
  }

  final def resetTimers(req1: Requirement, reqs: Requirement*): Unit = remote {
    log.info("Resetting timers")
    replyTo.send(Message.obtain(null, RESET_TIMERS)
      .withData(new Bundle().putArray(FOR, (req1 +: reqs).toArray)).withID(id))
  }

  final def onBind(intent: Intent): IBinder = try {
    if (UUID.fromString(intent.getAction) == token) {
      log.info("Binding plugin to main app")
      handler.getBinder
    } else null
  } catch {
    case _: NullPointerException | _: IllegalArgumentException => null
  }

  override final def onUnbind(intent: Intent): Boolean = try {
    if (UUID.fromString(intent.getAction) == token) remote {
      log.info("Unbinding plugin from main app")
      replyTo.send(Message.obtain(null, DEREGISTER).withID(id))
      stopSelf()
    }

    false
  } catch {
    case _: NullPointerException | _: IllegalArgumentException => false
  }

  override final def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    remote {
      for (
        clazz <- intent.get[Class[_]](EXECUTOR) if classOf[RulesExecutor].isAssignableFrom(clazz);
        gateway <- intent.get[Messenger](REPLY_TO)
      ) {
        replyTo = gateway
        register()
      }
    }

    Service.START_NOT_STICKY
  }
}

object Plugin {
  val CATEGORY = "sta.plugin"

  val EXECUTOR = "sta.app.main"
  val REPLY_TO = "sta.app.gateway"

  val REGISTER = 1
  val TOKEN = "sta.plugin.token"
  val ACTION_PARSER = "sta.parser.action"
  val TRIGGER_PARSER = "sta.parser.trigger"

  val DEREGISTER = 2

  val EXECUTE_ACTION = 3
  val ACTION = "sta.action.instance"
  val ACTION_EXCEPTION = "sta.action.exception"
  
  val UPDATE_STATE = 4
  val MODEL = "sta.model.instance"

  val RESET_TIMERS = 5
  val FOR = "sta.model.requirements"
}
