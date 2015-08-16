package sta.tests

import android.app.Activity
import android.content.{Context, ComponentName, ServiceConnection, Intent}
import android.content.pm.ActivityInfo
import android.net.Uri
import android.os.{Messenger, IBinder, Bundle}
import java.io.{FileOutputStream, File}
import kj.android.logging.Logging
import sta.services.STAService

class TestActivity extends Activity with Logging {
  private[this] var messenger: Option[Messenger] = None

  private lazy val files = {
    val assets = getResources.getAssets
    assets.list("main").map { a =>
      val tmp = File.createTempFile(a, ".rule", getCacheDir)
      val content = io.Source.fromInputStream(assets.open(s"main/$a")).mkString
      val fos = new FileOutputStream(tmp)
      try {
        fos.write(content.getBytes)
      } finally {
        fos.close()
      }
      Uri.fromFile(tmp)
    }
  }

  private val connection = new ServiceConnection {
    def onServiceConnected(name: ComponentName, service: IBinder): Unit = {
      val m = new Messenger(service)
      messenger = Some(m)
      m.send(STAService.loadRules(files: _*))
    }

    def onServiceDisconnected(name: ComponentName): Unit = {
      messenger = None
    }
  }

  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    if (getRequestedOrientation == ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE)
      setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE)
    else
      setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT)

    startService(new Intent(this, classOf[STAService]))
    bindService(new Intent(this, classOf[STAService]), connection, Context.BIND_AUTO_CREATE)
  }

  override def onDestroy(): Unit = {
    super.onDestroy()

    messenger.foreach(_ => unbindService(connection))
    files.foreach(uri => new File(uri.getPath).delete())
  }
}
