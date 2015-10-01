package sta.concurrent

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.util.control.NonFatal

sealed trait Atomic[T] {
  def get: T

  def set(newValue: T): Unit

  def compareAndSet(expect: T, update: T): Boolean

  @inline final def update(f: T => T): Either[Throwable, T] = {
    @inline @tailrec def set(): T = {
      val oldValue = get
      val newValue = f(oldValue)
      if (!compareAndSet(oldValue, newValue)) set()
      else newValue
    }

    try {
      Right(concurrent.blocking(set()))
    } catch {
      case NonFatal(th) => Left(th)
    }
  }
}

object Atomic {
  def apply[T](value: T): Atomic[T] = new Atomic[T] {
    private[this] val ref = new AtomicReference[T](value)

    @inline final def get: T = ref.get()

    @inline final def set(newValue: T): Unit = ref.set(newValue)

    @inline final def compareAndSet(expect: T, update: T): Boolean = ref.compareAndSet(expect, update)
  }
}
