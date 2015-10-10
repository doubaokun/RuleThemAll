package rta.tests

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import java.io.{File, FileOutputStream}
import rta.logging.Logging
import rta.tests.benchmarks.ParserBenchmark

class BenchmarkActivity extends Activity with Logging {
  val RULE_LOAD_REQUEST = 1

  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)

    setContentView(R.layout.activity_benchmark)
  }

  def runCustomParserBenchmark(view: View): Unit = {
    val chooseRuleIntent = new Intent(Intent.ACTION_GET_CONTENT)
    chooseRuleIntent.setType("text/*")
    chooseRuleIntent.putExtra(Intent.EXTRA_LOCAL_ONLY, true)
    startActivityForResult(chooseRuleIntent, RULE_LOAD_REQUEST)
  }

  def runParserBenchmark(view: View): Unit = {
    new BenchmarkTask(this, R.id.predefinedParserBenchmark,
      () => new ParserBenchmark(getResources.getAssets).runPredefined()
    ).execute()
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent): Unit = {
    requestCode match {
      case RULE_LOAD_REQUEST if resultCode == Activity.RESULT_OK =>
        new BenchmarkTask(this, R.id.customParserBenchmark,
          () => new ParserBenchmark(getResources.getAssets).runCustom(new File(data.getData.getPath))
        ).execute()
      case _ =>
    }
  }
}
