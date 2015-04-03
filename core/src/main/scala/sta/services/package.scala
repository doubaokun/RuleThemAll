package sta

import sta.common.Common
import kj.android.logging.LogTag
import sta.model.Model

import scala.collection.GenTraversable
import scala.language.implicitConversions

package object services extends Common {
  implicit val logTag = LogTag("sta.services")

  implicit def liftModelToList[M <: Model](m: M): List[M] = m :: Nil
}
