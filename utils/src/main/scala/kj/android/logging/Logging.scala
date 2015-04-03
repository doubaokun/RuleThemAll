package kj.android.logging

trait Logging {
  private implicit val logTag = new LogTag(this.getClass.getName.substring(0, 23))

  protected def log = Logger
}
