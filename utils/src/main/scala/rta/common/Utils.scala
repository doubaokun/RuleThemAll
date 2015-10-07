package rta.common

object Utils {
  @inline def withGC[T](body: => T) = {
    val result = body
    val v = System.getProperty("java.vm.version")
    if (v != null && v >= "2.0.0") System.gc()
    result
  }
}
