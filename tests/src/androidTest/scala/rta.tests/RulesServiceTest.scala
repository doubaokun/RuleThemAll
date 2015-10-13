package rta.tests

import android.content.{ServiceConnection, BroadcastReceiver, Intent}
import android.net.Uri
import android.os.Messenger
import android.test.ServiceTestCase
import android.util.SparseArray
import java.io.{File, FileOutputStream}
import org.scalatest.Matchers
import scala.collection.mutable
import scala.concurrent.duration._
import rta.common.Reflect._
import rta.common.Uses
import rta.model.triggers.Implicits._
import rta.model.triggers.Trigger
import rta.model.triggers.Trigger.Branch
import rta.parser.{ActionParser, TriggerParser, RulesParser}
import rta.parser.actions.ActionParsers
import rta.parser.triggers.TriggerParsers
import rta.service.{PluginHandler, RulesService}
import rta.storage.RulesStorage
import rta.tests.plugin.ExamplePlugin
import rta.tests.plugin.ExamplePlugin.{Dummy, ShowToast}

class RulesServiceTest extends ServiceTestCase[RulesService](classOf[RulesService]) with Matchers {
  def wait(max: Duration, samplePeriod: Duration)(breakWhen: => Boolean): Unit = {
    def loop(left: Duration): Unit = {
      if (left != Duration.Zero && !breakWhen) {
        Thread.sleep(samplePeriod.toMillis)
        loop(left - samplePeriod)
      }
    }
    loop(max)
  }

  def testServiceFlow(): Unit = {
    val pkg = "rta.tests"
    val ctx = new ExampleContext[ExamplePlugin](pkg, getSystemContext)
    setContext(ctx)

    // launch service
    val launch = new Intent(getSystemContext, classOf[RulesService])
    val binder = bindService(launch)
    binder shouldNot be (null)
    val service = new Messenger(binder)

    // plugin list should be empty at first
    val pluginUpdater = classOf[PluginHandler].reflect[BroadcastReceiver](getService)
      .`rta$service$PluginHandler$$pluginUpdater`()
    val plugins = classOf[PluginHandler].reflect[SparseArray[ServiceConnection]](getService)
      .`rta$service$PluginHandler$$plugins`()
    plugins.size should === (0)

    // remember amount of trigger amd action parsers
    val actionParsers = classOf[ActionParsers].reflect[mutable.LinkedHashMap[Class[_], ActionParser[_]]](RulesParser)
      .`rta$parser$actions$ActionParsers$$parsers`()
    val triggerParsers = classOf[TriggerParsers].reflect[mutable.LinkedHashMap[String, TriggerParser[_]]](RulesParser)
      .`rta$parser$triggers$TriggerParsers$$parsers`()
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
    service.send(RulesService.loadRules(files: _*))
    val storage = classOf[RulesService].reflect[RulesStorage](getService).`storage`
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
