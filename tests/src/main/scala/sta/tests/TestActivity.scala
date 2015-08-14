package sta.tests

import android.app.Activity
import android.content.Intent
import android.content.pm.ActivityInfo
import android.os.Bundle
import kj.android.logging.Logging
import sta.services.STAService

class TestActivity extends Activity with Logging {
  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    if (getRequestedOrientation == ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE)
      setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE)
    else
      setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT)

    startService(new Intent(this, classOf[STAService]))
  }
}
