package sta

import scala.language.implicitConversions

import kj.android.logging.LogTag
import sta.common.Common
import sta.model.Model

package object services extends Common {
  implicit val logTag = LogTag("sta.services")

  implicit def liftModel[M <: Model](m: M): Option[M] = Option(m)
}
