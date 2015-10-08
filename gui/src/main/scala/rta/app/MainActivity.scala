package rta.app

import android.app.{Activity, PendingIntent}
import android.content.{ComponentName, Context, Intent, ServiceConnection}
import android.graphics.Color
import android.graphics.drawable.ColorDrawable
import android.net.Uri
import android.os._
import android.view.ViewGroup.LayoutParams
import android.view.{ViewGroup, Gravity, View}
import android.widget._
import macroid.FullDsl._
import macroid._
import macroid.contrib.{ListTweaks, TextTweaks}
import macroid.viewable._
import scala.annotation.tailrec
import scala.util.control.NonFatal
import rta.common.Common
import rta.logging.Logging
import rta.service.RulesService

class MainActivity extends Activity with Logging with Contexts[Activity] with IdGeneration with Common { root =>
  val RULE_LOAD_REQUEST = 1
  val SERVICE_ON = "rta.service.state"

  private[this] var messenger: Option[Messenger] = None

  private[this] val sender = new Messenger(new Handler() {
    override def handleMessage(msg: Message): Unit = msg.what match {
      case RulesService.LIST =>
        for (arr <- msg.get[Array[String]](RulesService.NAMES)) {
          rulesSource.swap(arr.toSet)

          val seq = rulesSource.getCount match {
            case 0 => Ui.sequence(rawUnloadButton <~ disable, rawRules <~ hide)
            case c if c < 10 => Ui.sequence(rawUnloadButton <~ enable, rawRules <~ show)
            case _ =>
              Ui.sequence(
                rawUnloadButton <~ enable,
                rawRules <~ show,
                rawRulesList <~ Tweak[ListView] { lv =>
                  val item = rulesSource.getView(0, null, lv)
                  item.measure(0, 0)
                  (lp[LinearLayout](LayoutParams.MATCH_PARENT, (10.5 * item.getMeasuredHeight).toInt) +
                    setMargins(10, 20, 0, 0))(lv)
                }
              )
          }
          runUi(seq)
        }
      case _ => log.warn(s"Unknown $msg", new RuntimeException)
    }
  })

  private[this] val connection = new ServiceConnection {
    def onServiceConnected(name: ComponentName, service: IBinder): Unit = {
      log.info(s"Connected to service $name")
      messenger = Option(service).map(new Messenger(_))
      messenger.foreach(_.send(Message.obtain(null, RulesService.SUBSCRIBE).withSender(sender)))
    }

    def onServiceDisconnected(name: ComponentName): Unit = {
      log.info(s"Disconnected from service $name")
      messenger = None
    }
  }

  private[this] lazy val prefs = getPreferences(Context.MODE_PRIVATE)

  private[this] lazy val self = PendingIntent.getActivity(this, 0, new Intent(this, this.getClass), 0)

  private[this] lazy val rulesSource = {
    implicit def listable = ListableViewModifier.fromListable(Listable.text(TextTweaks.medium)) {
      case (parent, position, view) =>
        if (parent.isItemChecked(position)) {
          view <~ Tweak[TextView](v => v.setBackgroundColor(findSuitableBackgroundColor(v.getHighlightColor)))
        } else {
          view <~ Tweak[TextView](_.setBackgroundColor(parent.getSolidColor))
        }
    }

    new ListableStableAdapter[String, TextView](Set.empty)
  }

  private[this] var rawServiceSwitch = slot[Switch]
  private[this] var rawLoadButton = slot[Button]
  private[this] var rawRules = slot[LinearLayout]
  private[this] var rawRulesList = slot[ListView]
  private[this] var rawUnloadButton = slot[Button]

  def onCheck(handler: Boolean => Any) = Tweak[CompoundButton](_.setOnCheckedChangeListener {
    new CompoundButton.OnCheckedChangeListener {
      def onCheckedChanged(b: CompoundButton, c: Boolean) = handler(c)
    }
  })

  def onItemClick(handler: (AdapterView[_], View, Int, Long) => Unit) = Tweak[AdapterView[_]](_.setOnItemClickListener {
    new AdapterView.OnItemClickListener {
      def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = handler(parent, view, position, id)
    }
  })

  def addRules(rules: Int*) = Tweak[View] { view =>
    val lp = view.getLayoutParams.asInstanceOf[RelativeLayout.LayoutParams]
    rules.foreach(lp.addRule)
  }

  def setMargins(left: Int, top: Int, right: Int, bottom: Int) = Tweak[View] { view =>
    view.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams].setMargins(left, top, right, bottom)
  }

  def findSuitableBackgroundColor(color: Int) = {
    val hsv = new Array[Float](3)
    Color.RGBToHSV(Color.red(color), Color.green(color), Color.blue(color), hsv)
    if (hsv(2) < 0.5) {
      hsv(2) = 0.7f
    } else {
      hsv(2) = 0.3f
    }
    hsv(1) = hsv(1) * 0.2f
    Color.HSVToColor(hsv)
  }

  def serviceSwitch = text(R.string.app_service) + TextTweaks.medium + onCheck {
    case true =>
      if (!prefs.getBoolean(SERVICE_ON, false))
        startService(new Intent(root, classOf[RulesService]).putExtra(RulesService.BACKGROUND_ACTION, self))
      bindService(new Intent(root, classOf[RulesService]), connection, Context.BIND_AUTO_CREATE)
      runUi(List(rawLoadButton, rawUnloadButton) <~ show)
      rawServiceSwitch.foreach(s => prefs.edit().putBoolean(SERVICE_ON, true).apply())
    case false =>
      rulesSource.clear()
      unbindService(connection)
      stopService(new Intent(root, classOf[RulesService]))
      runUi(List(rawLoadButton, rawRules) <~ hide, rawUnloadButton <~ disable <~ hide)
      rawServiceSwitch.foreach(s => prefs.edit().putBoolean(SERVICE_ON, false).apply())
  } + lp[RelativeLayout](LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT) +
    addRules(RelativeLayout.ALIGN_PARENT_TOP, RelativeLayout.ALIGN_PARENT_RIGHT) +
    setMargins(10, 0, 0, 0)

  def loadButton = text(R.string.load_rules) + On.click(Ui {
    val chooseRuleIntent = new Intent(Intent.ACTION_GET_CONTENT)
    chooseRuleIntent.setType("text/*")
    chooseRuleIntent.putExtra(Intent.EXTRA_LOCAL_ONLY, true)
    chooseRuleIntent.putExtra(Intent.EXTRA_ALLOW_MULTIPLE, true)
    startActivityForResult(chooseRuleIntent, RULE_LOAD_REQUEST)
  }) + lp[RelativeLayout](LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT) +
    addRules(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.ALIGN_PARENT_RIGHT)

  def rules = lp[RelativeLayout](LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT) +
    addRules(RelativeLayout.CENTER_IN_PARENT) + vertical

  def rulesHeader = text(R.string.rules_list) + TextTweaks.medium +
    Tweak[TextView](_.setGravity(Gravity.CENTER_HORIZONTAL)) +
    lp[LinearLayout](LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT) + setMargins(10, 0, 0, 0)

  def rulesList = ListTweaks.adapter(rulesSource) + Tweak[ListView] { lv =>
    lv.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE)
    lv.setDivider(new ColorDrawable(Color.TRANSPARENT))
  } + onItemClick {
    case (parent: ListView, view: TextView, pos, id) =>
      if (parent.isItemChecked(pos)) {
        view.setBackgroundColor(findSuitableBackgroundColor(view.getHighlightColor))
      } else {
        view.setBackgroundColor(parent.getSolidColor)
      }
    case _ =>
  } + lp[LinearLayout](LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT) + setMargins(10, 20, 0, 0)

  def unloadButton = text(R.string.unload_rules) + On.click(Ui {
    for (m <- messenger; lv <- rawRulesList if lv.getCheckedItemCount > 0) {
      val checked = lv.getCheckedItemIds
      val msg = RulesService.unloadRules(checked.map(rulesSource.apply): _*)
      m.send(msg)
    }
  }) + lp[RelativeLayout](LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT) +
    addRules(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.ALIGN_PARENT_LEFT)

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent): Unit = try {
    requestCode match {
      case RULE_LOAD_REQUEST if resultCode == Activity.RESULT_OK && data.getClipData != null =>
        val clipData = data.getClipData

        @tailrec def rec(idx: Int, uris: List[Uri]): List[Uri] = {
          if (idx < clipData.getItemCount) {
            rec(idx + 1, Uri.parse(clipData.getItemAt(idx).getUri.getPath) :: uris)
          } else uris
        }

        val paths = rec(idx = 0, uris = Nil)
        messenger.foreach { m =>
          log.info(s"Loading rules from $paths")
          m.send(RulesService.loadRules(paths: _*))
        }
      case RULE_LOAD_REQUEST if resultCode == Activity.RESULT_OK =>
        val path = Uri.parse(data.getData.getPath)
        messenger.foreach { m =>
          log.info(s"Loading rules from $path")
          m.send(RulesService.loadRules(path))
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
          w[Button] <~ wire(rawLoadButton) <~ loadButton <~ hide,
          w[Button]  <~ wire(rawUnloadButton) <~ unloadButton <~ disable <~ hide,
          l[LinearLayout](
            w[TextView] <~ rulesHeader,
            w[ListView] <~ wire(rawRulesList) <~ rulesList
          ) <~ wire(rawRules) <~ rules <~ hide,
          w[Switch] <~ wire(rawServiceSwitch) <~ serviceSwitch
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

    messenger.foreach(_.send(Message.obtain(null, RulesService.UNSUBSCRIBE).withSender(sender)))
    unbindService(connection)
  }
}
