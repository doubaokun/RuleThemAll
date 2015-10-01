package sta.tests

import android.content.{BroadcastReceiver, Intent}
import android.net.Uri
import android.os.Messenger
import android.test.ServiceTestCase
import android.util.SparseArray
import java.io.{File, FileOutputStream}
import org.scalatest.Matchers
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.{ClassTag, classTag}
import sta.common.Uses
import sta.model.triggers.Implicits._
import sta.model.triggers.Trigger
import sta.model.triggers.Trigger.Branch
import sta.parser.RulesParser
import sta.parser.actions.ActionRules
import sta.parser.triggers.TriggerRules
import sta.service.{PluginHandler, STAService}
import sta.storage.RulesStorage
import sta.tests.plugin.ExamplePlugin
import sta.tests.plugin.ExamplePlugin.{Dummy, ShowToast}

class STAServiceTest extends ServiceTestCase[STAService](classOf[STAService]) with Matchers {
  def wait(max: Duration, samplePeriod: Duration)(breakWhen: => Boolean): Unit = {
    def loop(left: Duration): Unit = {
      if (left != Duration.Zero && !breakWhen) {
        Thread.sleep(samplePeriod.toMillis)
        loop(left - samplePeriod)
      }
    }
    loop(max)
  }

  def callPrivateField[T: ClassTag, R](name: String, obj: Any) = {
    val f = classTag[T].runtimeClass.getDeclaredField(name)
    f.setAccessible(true)
    f.get(obj).asInstanceOf[R]
  }

  def callPrivateMethod[T: ClassTag, R](name: String, obj: T, args: AnyRef*) = {
    val m = classTag[T].runtimeClass.getDeclaredMethod(name)
    m.setAccessible(true)
    m.invoke(obj, args: _*).asInstanceOf[R]
  }
  
  def testServiceFlow(): Unit = {
    val pkg = "sta.tests"
    val ctx = new ExampleContext[ExamplePlugin](pkg, getSystemContext)
    setContext(ctx)

    // launch service
    val launch = new Intent(getSystemContext, classOf[STAService])
    val binder = bindService(launch)
    binder shouldNot be (null)
    val service = new Messenger(binder)

    // plugin list should be empty at first
    val pluginUpdater = callPrivateMethod[PluginHandler, BroadcastReceiver](
      "sta$service$PluginHandler$$pluginUpdater", getService)
    val plugins = callPrivateMethod[PluginHandler, SparseArray[_]](
      "sta$service$PluginHandler$$plugins", getService)
    plugins.size should === (0)

    // remember amount of trigger amd action parsers
    val actionParsers = callPrivateMethod[ActionRules, mutable.HashMap[_, _]](
      "sta$parser$actions$ActionRules$$parsers", RulesParser
    )
    val triggerParsers = callPrivateMethod[TriggerRules, mutable.HashMap[_, _]](
      "sta$parser$triggers$TriggerRules$$parsers", RulesParser
    )
    val actionParsersSize = actionParsers.size
    val triggerParsersSize = triggerParsers.size

    // load plugin
    val add = new Intent(Intent.ACTION_PACKAGE_ADDED).setData(Uri.parse(s"package:$pkg"))
    pluginUpdater.onReceive(getSystemContext, add)
    plugins.size should === (1)
    actionParsers.size should === (actionParsersSize + 1)
    triggerParsers.size should === (triggerParsersSize + 1)

    // load rule
    val files = {
      val assets = getSystemContext.getResources.getAssets
      assets.list("plugin").map { a =>
        val tmp = File.createTempFile(a, ".rule", getSystemContext.getCacheDir)
        val content = io.Source.fromInputStream(assets.open(s"plugin/$a")).mkString
        val fos = new FileOutputStream(tmp)
        try {
          fos.write(content.getBytes)
        } finally {
          fos.close()
        }
        Uri.fromFile(tmp)
      }
    }
    service.send(STAService.loadRules(files: _*))
    val storage = callPrivateField[STAService, RulesStorage](
      "sta$service$STAService$$storage", getService)
    wait(30.seconds, 250.millis)(storage.allRules.nonEmpty)
    val rules = storage.allRules.toList
    rules.size should === (1)
    val rule = rules.head
    rule.branches should contain theSameElementsAs Seq(
      Branch(
        timers = Seq.empty,
        conditions = Seq(Trigger.Condition[Dummy](_ == Dummy()))
      ),
      Branch(
        timers = Seq(Trigger.Timer.dynamic(24.hours, implicitly[Uses[CalendarEvent]].requirements) { case _ => None }),
        conditions = Seq.empty
      )
    )
    rule.actions should contain theSameElementsAs Seq(ShowToast("Hello world!"))

    // send RESET_TIMERS command
    val requirements = implicitly[Uses[CalendarEvent]].requirements.toSeq
    ctx.service.resetTimers(requirements.head, requirements.tail: _*)
    wait(10.seconds, 1.second)(false)

    // reload plugin
    val replace = new Intent(Intent.ACTION_PACKAGE_REPLACED).setData(Uri.parse(s"package:$pkg"))
    pluginUpdater.onReceive(getSystemContext, replace)
    plugins.size should ===(1)

    // send UPDATE_MODEL command
    ctx.service.update(ExamplePlugin.Dummy())
    wait(10.seconds, 1.second)(false)

    // remove plugin
    val remove = new Intent(Intent.ACTION_PACKAGE_REMOVED).setData(Uri.parse(s"package:$pkg"))
    pluginUpdater.onReceive(getSystemContext, remove)
    plugins.size should ===(0)
    actionParsers.size should === (actionParsersSize)
    triggerParsers.size should === (triggerParsersSize)
  }
}
