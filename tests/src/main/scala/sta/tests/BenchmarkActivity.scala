package sta.tests

import android.app.Activity
import android.content.pm.ActivityInfo
import android.os.{ StrictMode, Bundle }
import sta.tests.benchmarks.ParserBenchmark

class BenchmarkActivity extends Activity {
  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    if (getRequestedOrientation == ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE)
      setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE)
    else
      setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT)

    StrictMode.setThreadPolicy(new StrictMode.ThreadPolicy.Builder().permitAll().build())

    new ParserBenchmark(getResources.getAssets)

    finish()
  }
}
