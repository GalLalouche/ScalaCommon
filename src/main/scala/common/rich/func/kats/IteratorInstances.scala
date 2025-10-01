package common.rich.func.kats

import cats.{Functor, FunctorFilter, Monad, MonoidK, UnorderedFoldable}
import cats.implicits.catsSyntaxSemigroup
import cats.kernel.CommutativeMonoid

trait IteratorInstances {
  implicit object iteratorInstances
      extends MonoidK[Iterator]
      with UnorderedFoldable[Iterator]
      with Monad[Iterator]
      with FunctorFilter[Iterator] {
    override def empty[A]: Iterator[A] = Iterator.empty
    override def unorderedFoldMap[A, B: CommutativeMonoid](fa: Iterator[A])(f: A => B): B =
      fa.foldLeft(CommutativeMonoid.empty)(_ |+| f(_))
    override def combineK[A](x: Iterator[A], y: Iterator[A]): Iterator[A] = x ++ y
    override def pure[A](x: A): Iterator[A] = Iterator(x)
    override def flatMap[A, B](fa: Iterator[A])(f: A => Iterator[B]): Iterator[B] = fa.flatMap(f)
    // While there are now laws testing this due to Iterator being mutable by definition, it is checked via
    // the iterable instances which uses the seeker as well.
    override def tailRecM[A, B](a: A)(f: A => Iterator[Either[A, B]]): Iterator[B] =
      new IteratorSeeker(a, f)
    // Optimized overrides
    override def map[A, B](fa: Iterator[A])(f: A => B): Iterator[B] = fa.map(f)
    override def functor: Functor[Iterator] = this
    override def mapFilter[A, B](fa: Iterator[A])(f: A => Option[B]): Iterator[B] = fa.flatMap(f(_))
  }
}

object IteratorInstances extends IteratorInstances
