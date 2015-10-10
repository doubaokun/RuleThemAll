package rta.model

package object actions {
  type ActionKind = (Class[_], Option[Any])

  implicit class BooleanOps(val v: Boolean) extends AnyVal {
    def toInt: Int = v match {
      case false => 0
      case true => 1
    }
  }
}
