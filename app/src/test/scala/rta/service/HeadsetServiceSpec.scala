package rta.service

import android.content.Intent
import org.robolectric.annotation.Config
import org.scalatest.{FlatSpec, Matchers, RobolectricSuite}
import rta.model.triggers.Implicits._

@Config(sdk = Array(21), manifest = "app/src/test/AndroidManifest.xml")
class HeadsetServiceSpec extends FlatSpec with RobolectricSuite with Matchers {

  trait Service {
    def prepareIntent(state: Int): Intent = {
      val intent = new Intent
      intent.setAction(Intent.ACTION_HEADSET_PLUG)
      intent.putExtra("state", state)
    }

    val service = new HeadsetService
  }

  behavior of "HeadsetService"

  it should "report connected headset" in new Service {
    service.handle(prepareIntent(Headset.Connected.intValue)) should
      ===(Some(Headset.Connected))
  }

  it should "report disconnected headset" in new Service {
    service.handle(prepareIntent(Headset.Disconnected.intValue)) should
      ===(Some(Headset.Disconnected))
  }
}
