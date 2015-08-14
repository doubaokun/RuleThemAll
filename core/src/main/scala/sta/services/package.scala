package sta

import scala.language.implicitConversions

import sta.common.Common
import sta.model.Model

package object services extends Common {
  implicit def liftModel[M <: Model](m: M): Option[M] = Option(m)
}
