package kj.android.common

final class Option[+A] private (val value: Any) extends AnyVal {
  @inline private def unsafeGet: A = value.asInstanceOf[A]

  @inline def isEmpty: Boolean = value == null

  @inline def isDefined: Boolean = !isEmpty

  @inline def nonEmpty = isDefined

  @inline def get: A =
    if (isDefined) value.asInstanceOf[A] else throw new NoSuchElementException("None.get")

  @inline def getOrElse[U >: A](default: => U): U = if (isEmpty) default else unsafeGet

  @inline def orNull[U >: A](implicit ev: Null <:< U): U = getOrElse(ev(null))

  @inline def ===[B >: A](v: B): Boolean = contains(v)

  @inline def contains[B >: A](elem: B): Boolean = !isEmpty && value == elem

  @inline def exists(p: A => Boolean): Boolean = !isEmpty && p(unsafeGet)

  @inline def forall(p: A => Boolean): Boolean = isEmpty || p(unsafeGet)

  @inline def filter(p: A => Boolean): Option[A] =
    if (isEmpty || p(unsafeGet)) this else Option.empty[A]

  @inline def filterNot(p: A => Boolean): Option[A] =
    if (isEmpty || !p(unsafeGet)) this else Option.empty[A]

  @inline def withFilter(f: A => Boolean): Option[A] = filter(f)

  @inline def map[B](f: A => B): Option[B] =
    if (isEmpty) Option.empty[B] else Option.apply[B](f(unsafeGet))

  @inline def flatMap[B](f: A => Option[B]): Option[B] =
    if (isEmpty) Option.empty[B] else f(unsafeGet)

  @inline def collect[B](pf: PartialFunction[A, B]): Option[B] =
    if (!isEmpty && pf.isDefinedAt(get)) Option.apply[B](pf(unsafeGet)) else Option.empty[B]

  @inline def fold[B](ifEmpty: => B)(f: A => B): B = if (isEmpty) ifEmpty else f(unsafeGet)

  @inline def foreach[B](f: A => B): Unit = if (!isEmpty) f(unsafeGet)

  @inline def flatten[B](implicit ev: A <:< Option[B]): Option[B] =
    if (isEmpty) Option.empty[B] else ev(unsafeGet)

  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(unsafeGet)

  def toList: List[A] = if (isEmpty) List() else new ::(unsafeGet, Nil)

  def toRight[X](left: => X) = if (isEmpty) Left(left) else Right(value)

  def toLeft[X](right: => X) = if (isEmpty) Right(right) else Left(value)

  def toScalaOption: scala.Option[A] = scala.Option(value.asInstanceOf[A])

  override def toString = if (isEmpty) "Option.none" else "Some(" + unsafeGet + ")"
}

object Option {

  import scala.language.implicitConversions

  implicit def option2Iterable[A](opt: Option[A]): Iterable[A] = opt.toList

  @inline def apply[A](value: A) = new Option[A](value)

  @inline def empty[A] = new Option[A](null)
}
