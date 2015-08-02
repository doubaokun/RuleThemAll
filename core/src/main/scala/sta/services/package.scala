package sta

import scala.language.implicitConversions

import kj.android.logging.LogTag
import sta.common.Common
import sta.model.Model

package object services extends Common {
  implicit val logTag = LogTag("sta.services")

  implicit def liftModelToList[M <: Model](m: M): List[M] = m :: Nil
}
