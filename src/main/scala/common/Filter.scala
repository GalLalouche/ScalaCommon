package common

import common.rich.primitives.RichBoolean.richBoolean

trait Filter[-A] {
  def passes(a: A): Boolean
  @inline def fails(a: A): Boolean = passes(a).isFalse
  @inline def and[B <: A](other: Filter[B]): Filter[B] = e => passes(e) && other.passes(e)
  @inline def &&[B <: A](other: Filter[B]): Filter[B] = and(other)
  @inline def or[B <: A](other: Filter[B]): Filter[B] = e => passes(e) || other.passes(e)
  @inline def ||[B <: A](other: Filter[B]): Filter[B] = or(other)
  @inline def negate: Filter[A] = fails(_)
  @inline def ! : Filter[A] = negate
}

object Filter {
  def always: Filter[Any] = _ => true
  def never: Filter[Any] = _ => false
}
