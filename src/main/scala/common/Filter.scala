package common

import common.rich.primitives.RichBoolean.richBoolean

trait Filter[-A] {
  def passes(a: A): Boolean
  @inline final def fails(a: A): Boolean = passes(a).isFalse
  @inline def and[B <: A](other: Filter[B]): Filter[B] = e => passes(e) && other.passes(e)
  @inline final def &&[B <: A](other: Filter[B]): Filter[B] = and(other)
  @inline def or[B <: A](other: Filter[B]): Filter[B] = e => passes(e) || other.passes(e)
  @inline final def ||[B <: A](other: Filter[B]): Filter[B] = or(other)
  @inline def negate: Filter[A] = fails(_)
  @inline final def ! : Filter[A] = negate
}

object Filter {
  def always: Filter[Any] = new Filter[Any] {
    override def passes(a: Any): Boolean = true
    override def negate: Filter[Any] = never
    override def and[B <: Any](other: Filter[B]): Filter[B] = other
    override def or[B <: Any](other: Filter[B]): Filter[B] = this
  }
  def never: Filter[Any] = new Filter[Any] {
    override def passes(a: Any): Boolean = false
    override def negate: Filter[Any] = always
    override def and[B <: Any](other: Filter[B]): Filter[B] = this
    override def or[B <: Any](other: Filter[B]): Filter[B] = other
  }
}
