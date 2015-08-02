package sta.services

import android.content.Intent
import org.robolectric.annotation.Config
import org.scalatest.{ Matchers, RobolectricSuite, WordSpec }
import sta.model.system._

@Config(sdk = Array(18), manifest = "core/src/test/AndroidManifest.xml")
class HeadsetServiceSpec extends WordSpec with RobolectricSuite with Matchers {

  private def prepareIntent(connected: Boolean): Intent = {
    val intent = new Intent
    intent.setAction(Intent.ACTION_HEADSET_PLUG)
    intent.putExtra("state", if (connected) 1 else 0)
  }

  "HeadsetService" should {
    val service = new HeadsetService {}

    "report connected headset" in {
      service.handle(prepareIntent(connected = true)) should ===(Headset.Connected :: Nil)
    }

    "report disconnected headset" in {
      service.handle(prepareIntent(connected = false)) should ===(Headset.Disconnected :: Nil)
    }
  }
}
