package rta.tests

import android.app.Activity
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.view.View
import java.io.{File, FileOutputStream}
import rta.logging.Logging
import rta.tests.benchmarks.ParserBenchmark
import org.scalameter.utils.IO

class BenchmarkActivity extends Activity with Logging {
  val RULE_LOAD_REQUEST = 1

  lazy val filesDir = new File(getExternalFilesDir(null), "benchmarks")

  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)

    filesDir.mkdirs()
    val assets = getResources.getAssets
    val buffer = new Array[Byte](2048)
    assets.list("benchmarks").foreach { file =>
      val is = assets.open(s"benchmarks/$file")
      val os = new FileOutputStream(new File(filesDir, file))

      try {
        var bytes = is.read(buffer)
        while(bytes != -1) {
          os.write(buffer, 0, bytes)
          bytes = is.read(buffer)
        }
      } finally {
        is.close()
        os.close()
      }
    }

    setContentView(R.layout.activity_benchmark)
  }

  def runParserBenchmark(view: View): Unit = {
    log.info(s"Choosing file for parser benchmark in ${filesDir.getPath}")
    val chooseRuleIntent = new Intent(Intent.ACTION_GET_CONTENT)
    chooseRuleIntent.setType("text/*")
    chooseRuleIntent.putExtra(Intent.EXTRA_LOCAL_ONLY, true)
    startActivityForResult(chooseRuleIntent, RULE_LOAD_REQUEST)
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent): Unit = {
    requestCode match {
      case RULE_LOAD_REQUEST if resultCode == Activity.RESULT_OK =>
        new BenchmarkTask(this, R.id.parserBenchmark,
          () => new ParserBenchmark(getResources.getAssets).run(new File(data.getData.getPath))
        ).execute()
      case _ =>
    }
  }

  override def onDestroy(): Unit = {
    super.onDestroy()

    def delete(parent: File): Unit = {
      parent.listFiles().foreach(delete)
      parent.delete()
    }
    delete(filesDir)
  }
}
