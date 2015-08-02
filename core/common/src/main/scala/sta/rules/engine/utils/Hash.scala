package sta.rules.engine.utils

trait Hash[T] {
  def hash: Long
}

object Hash {
  def neutral: Long = 0

  @inline private[engine] def tripleHash(a: Any, b: Any, c: Any): Long = a.##.toLong ^ b.## ^ c.##
}
