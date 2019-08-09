package common.rich.primitives

object RichBoolean {
  implicit class richBoolean(private val b: Boolean) extends AnyVal {
    @inline def ifTrue[T](t: => T): Option[T] = if (b) Some(t) else None
    @inline def ifFalse[T](t: => T): Option[T] = isFalse ifTrue t
    @inline def isFalse: Boolean = !b
    @inline def xor(other: Boolean): Boolean = b != other
    @inline def ⊕(other: Boolean): Boolean = xor(other)
  }

  def or[A](p1: A => Boolean, rest: (A => Boolean)*): A => Boolean = a => p1(a) || rest.exists(_ (a))
  def and[A](p1: A => Boolean, rest: (A => Boolean)*): A => Boolean = a => p1(a) && rest.forall(_ (a))
}
