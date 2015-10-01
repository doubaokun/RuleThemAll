package sta.tests

import android.app.Activity
import android.os.Bundle
import android.view.View
import sta.logging.Logging
import sta.tests.benchmarks.ParserBenchmark

class BenchmarkActivity extends Activity with Logging {
  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)

    setContentView(R.layout.activity_benchmark)
  }

  def runParserBenchmark(view: View): Unit = {
    log.info("Running ParserBenchmark")
    new BenchmarkTask(this, R.id.parserBenchmark,
      () => new ParserBenchmark(getResources.getAssets).run()
    ).execute()
  }
}
