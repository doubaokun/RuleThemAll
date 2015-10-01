package sta.app

import android.app.{Activity, PendingIntent}
import android.content.{ComponentName, Context, Intent, ServiceConnection}
import android.net.Uri
import android.os._
import android.view.View
import android.view.ViewGroup.LayoutParams
import android.widget._
import macroid.FullDsl._
import macroid._
import macroid.contrib.{ListTweaks, TextTweaks}
import macroid.viewable._
import scala.annotation.tailrec
import scala.util.control.NonFatal
import sta.common.Common
import sta.logging.Logging
import sta.service.STAService

class MainActivity extends Activity with Logging with Contexts[Activity] with IdGeneration with Common { root =>
  val RULE_LOAD_REQUEST = 1
  val SERVICE_ON = "sta.service.state"

  private[this] var messenger: Option[Messenger] = None

  private[this] val sender = new Messenger(new Handler() {
    override def handleMessage(msg: Message): Unit = msg.what match {
      case STAService.LIST =>
        for(arr <- msg.get[Array[String]](STAService.names)) {
          rulesSource.swap(arr.toSet)
          runUi {
            if (!rulesSource.isEmpty) rawUnloadButton <~ enable else rawUnloadButton <~ disable
          }
        }
      case _ => log.warn(s"Unknown $msg", new RuntimeException)
    }
  })

  private[this] val connection = new ServiceConnection {
    def onServiceConnected(name: ComponentName, service: IBinder): Unit = {
      log.info(s"Connected to service $name")
      messenger = Option(service).map(new Messenger(_))
      messenger.foreach(_.send(Message.obtain(null, STAService.SUBSCRIBE).withSender(sender)))
    }

    def onServiceDisconnected(name: ComponentName): Unit = {
      log.info(s"Disconnected from service $name")
      messenger = None
    }
  }

  private[this] lazy val prefs = getPreferences(Context.MODE_PRIVATE)

  private[this] lazy val self = PendingIntent.getActivity(this, 0, new Intent(this, this.getClass), 0)

  // TODO custom adapter based on Set
  private[this] lazy val rulesSource = {
    implicit def listable = Listable.text(TextTweaks.medium)

    new ListableStableAdapter[String, TextView](Set.empty)
  }

  private[this] var rawServiceSwitch = slot[Switch]
  private[this] var rawLoadButton = slot[Button]
  private[this] var rawRulesList = slot[ListView]
  private[this] var rawUnloadButton = slot[Button]

  def onCheck(handler: Boolean => Any) = Tweak[CompoundButton](_.setOnCheckedChangeListener {
    new CompoundButton.OnCheckedChangeListener {
      def onCheckedChanged(b: CompoundButton, c: Boolean) = handler(c)
    }
  })

  def onItemClick(handler: (Int, Long) => Unit) = Tweak[AdapterView[_]](_.setOnItemClickListener {
    new AdapterView.OnItemClickListener {
      def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = handler(position, id)
    }
  })

  def relativeLayoutParams(w: Int, h: Int, rules: Int*) = {
    val params = new RelativeLayout.LayoutParams(w, h)
    rules.foreach(params.addRule)
    Tweak[View](_.setLayoutParams(params))
  }

  def serviceSwitch = text(R.string.app_service) + onCheck {
    case true =>
      if (!prefs.getBoolean(SERVICE_ON, false))
        startService(new Intent(root, classOf[STAService]).putExtra(STAService.BACKGROUND_ACTION, self))
      bindService(new Intent(root, classOf[STAService]), connection, Context.BIND_AUTO_CREATE)
      runUi(
        List(rawLoadButton, rawRulesList) <~ enable <~ show,
        rawUnloadButton <~ show
      )
      rawServiceSwitch.foreach(s => prefs.edit().putBoolean(SERVICE_ON, true).apply())
    case false =>
      rulesSource.clear()
      unbindService(connection)
      stopService(new Intent(root, classOf[STAService]))
      runUi {
        List(rawLoadButton, rawUnloadButton, rawRulesList) <~ disable <~ hide
      }
      rawServiceSwitch.foreach(s => prefs.edit().putBoolean(SERVICE_ON, false).apply())
  } + relativeLayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT,
    RelativeLayout.ALIGN_PARENT_TOP, RelativeLayout.ALIGN_PARENT_RIGHT)

  def loadButton = text(R.string.load_rules) + On.click(Ui {
    val choseRuleIntent = new Intent(Intent.ACTION_GET_CONTENT)
    choseRuleIntent.setType("*/*")
    choseRuleIntent.putExtra(Intent.EXTRA_LOCAL_ONLY, true)
    choseRuleIntent.putExtra(Intent.EXTRA_ALLOW_MULTIPLE, true)
    startActivityForResult(choseRuleIntent, RULE_LOAD_REQUEST)
  }) + relativeLayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT,
    RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.ALIGN_PARENT_RIGHT)

  def rulesList = ListTweaks.adapter(rulesSource) + Tweak[ListView] { lv =>
    lv.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE)
  } + relativeLayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT, RelativeLayout.CENTER_IN_PARENT)

  def unloadButton = text(R.string.unload_rules) + On.click(Ui {
    for (m <- messenger; lv <- rawRulesList if lv.getCheckedItemCount > 0) {
      val checked = lv.getCheckedItemIds
      val msg = STAService.unloadRules(checked.map(rulesSource.apply): _*)
      m.send(msg)
    }
  }) + relativeLayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT,
    RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.ALIGN_PARENT_LEFT)

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent): Unit = try {
    requestCode match {
      case RULE_LOAD_REQUEST if resultCode == Activity.RESULT_OK && data.getClipData != null =>
        val clipData = data.getClipData

        @tailrec def rec(idx: Int, uris: List[Uri] = Nil): List[Uri] = {
          if (idx < clipData.getItemCount) {
            rec(idx + 1, Uri.parse(clipData.getItemAt(idx).getUri.getPath) :: uris)
          } else uris
        }

        val paths = rec(idx = 0)
        messenger.foreach { m =>
          log.info(s"Loading rules from $paths")
          m.send(STAService.loadRules(paths: _*))
        }
      case RULE_LOAD_REQUEST if resultCode == Activity.RESULT_OK =>
        val path = Uri.parse(data.getData.getPath)
        messenger.foreach { m =>
          log.info(s"Loading rules from $path")
          m.send(STAService.loadRules(path))
        }
      case _ =>
    }
  } catch {
    case NonFatal(th) => log.error(s"Exception during handling ($requestCode, $resultCode, $data)", th)
  }

  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)

    setContentView {
      getUi {
        l[RelativeLayout](
          w[Switch] <~ wire(rawServiceSwitch) <~ serviceSwitch,
          w[Button] <~ wire(rawLoadButton) <~ loadButton <~ disable <~ hide,
          w[Button]  <~ wire(rawUnloadButton) <~ unloadButton <~ disable <~ hide,
          w[ListView] <~ wire(rawRulesList) <~ rulesList <~ disable <~ hide
        )
      }
    }

    rawServiceSwitch.foreach { switch =>
      if (prefs.contains(SERVICE_ON)) {
        switch.setChecked(prefs.getBoolean(SERVICE_ON, false))
      }
    }
  }

  override def onDestroy(): Unit = {
    super.onDestroy()

    unbindService(connection)
  }
}
