package kj.android.logging

trait Logging {
  implicit lazy val logTag = {
    val name = this.getClass.getSimpleName
    new LogTag(name.substring(0, math.min(name.length, 23)))
  }

  @inline protected def log = Logger
}
