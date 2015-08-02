package kj.android.logging

trait Logging {
  protected implicit lazy val logTag = {
    val name = this.getClass.getSimpleName
    new LogTag(name.substring(0, math.min(name.length, 23)))
  }

  protected def log = Logger
}
