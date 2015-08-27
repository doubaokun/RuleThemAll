package sta

import scala.language.implicitConversions
import android.content.{Intent, IntentFilter}
import android.os.PatternMatcher
import kj.android.common.Common
import sta.common.Requirement
import sta.model.Model

package object services extends Common {
  implicit def liftModel[M <: Model](m: M): Option[M] = Option(m)

  implicit def liftActionToRequirement(action: String): Requirement =
    new Requirement.IntentBased(new Intent(action))

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
