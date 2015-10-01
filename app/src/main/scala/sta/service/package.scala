package sta

import scala.language.implicitConversions
import android.content.{Intent, IntentFilter}
import android.os.PatternMatcher
import sta.common.{Common, Requirement}
import sta.model.{BaseModel, Model}

package object service extends Common {
  implicit def liftModel[M <: BaseModel](m: M): Option[M] = Option(m)

  implicit def liftActionToRequirement(action: String): Requirement =
    new Requirement.IntentBased(new Intent(action))

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Null"))
  implicit def intentToIntentFilter(intent: Intent): IntentFilter = {
    val f = new IntentFilter(intent.getAction)
    val uri = intent.getData
    if (uri != null) {
      if (uri.getScheme != null) f.addDataScheme(uri.getScheme)
      if (uri.getHost != null) {
        val port = uri.getPort
        f.addDataAuthority(uri.getHost, if (port == -1) null else port.toString)
      }
      if (uri.getPath != null) f.addDataPath(uri.getPath, PatternMatcher.PATTERN_LITERAL)
    }
    f
  }
}
