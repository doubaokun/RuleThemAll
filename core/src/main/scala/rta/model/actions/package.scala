package rta.model

package object actions {
  implicit class BooleanOps(val v: Boolean) extends AnyVal {
    def toInt: Int = v match {
      case false => 0
      case true => 1
    }
  }
}

package actions {
  final case class ActionKind(data: (Class[_], Option[Any]))
}
