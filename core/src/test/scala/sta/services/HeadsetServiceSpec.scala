package sta.services

import android.content.Intent
import org.robolectric.annotation.Config
import org.scalatest.{ Matchers, RobolectricSuite, WordSpec }
import sta.model.triggers.Implicits

@Config(sdk = Array(18), manifest = "core/src/test/AndroidManifest.xml")
class HeadsetServiceSpec extends WordSpec with RobolectricSuite with Matchers {

  private def prepareIntent(state: Int): Intent = {
    val intent = new Intent
    intent.setAction(Intent.ACTION_HEADSET_PLUG)
    intent.putExtra("state", state)
  }

  "HeadsetService" should {
    val service = new HeadsetService

    "report connected headset" in {
      service.handle(prepareIntent(Headset.Connected.intValue)) should
        === (Some(Headset.Connected))
    }

    "report disconnected headset" in {
      service.handle(prepareIntent(Headset.Disconnected.intValue)) should
        === (Some(Headset.Disconnected))
    }
  }
}
