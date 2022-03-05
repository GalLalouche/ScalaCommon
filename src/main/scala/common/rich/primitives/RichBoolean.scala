package common.rich.primitives

object RichBoolean {
  implicit class richBoolean(private val b: Boolean) extends AnyVal {
    @inline def ifTrue[T](t: => T): Option[T] = if (b) Some(t) else None
    @inline def ifFalse[T](t: => T): Option[T] = isFalse ifTrue t
    @inline def isFalse: Boolean = !b
    @inline def xor(other: Boolean): Boolean = b != other
    @inline def ⊕(other: Boolean): Boolean = xor(other)
    @inline def implies(other: Boolean): Boolean = !b || other
    @inline def ==>(other: Boolean): Boolean = implies(other)
    @inline def neither(other: Boolean): Boolean = !b && !other
  }

  def or[A](p1: A => Boolean, p2: A => Boolean, rest: (A => Boolean)*): A => Boolean =
    a => p1(a) || p2(a) || rest.exists(_ (a))
  def and[A](p1: A => Boolean, p2: A => Boolean, rest: (A => Boolean)*): A => Boolean =
    a => p1(a) && p2(a) && rest.forall(_ (a))
  def negate[A](p: A => Boolean): A => Boolean = p.andThen(_.isFalse)
}
