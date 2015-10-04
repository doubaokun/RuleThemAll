package sta.model

package object actions {
  implicit class BooleanOps(val v: Boolean) extends AnyVal {
    def toInt: Int = v match {
      case false => 0
      case true => 1
    }
  }
}
