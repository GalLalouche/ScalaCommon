package common.rich.func.scalazz

import scalaz.{Foldable, IsEmpty, MonadPlus, Monoid}

/** Has a non-overflowing implementation of foldl. */
trait BetterSetInstances {
  implicit object betterSetInstances
      extends MonadPlus[Set] /*Kinda*/
      with Foldable[Set]
      with IsEmpty[Set] {
    override def bind[A, B](fa: Set[A])(f: A => Set[B]) = fa.flatMap(f)
    override def point[A](a: => A) = Set(a)
    override def empty[A] = Set.empty
    override def plus[A](a: Set[A], b: => Set[A]) = a ++ b

    // Optimized implementation
    override def foldRight[A, B](fa: Set[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
    override def foldMap[A, B: Monoid](fa: Set[A])(f: A => B): B =
      fa.map(f).fold(Monoid[B].zero)(Monoid[B].append(_, _))
    override def foldLeft[A, B](fa: Set[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
    override def isEmpty[A](fa: Set[A]): Boolean = fa.isEmpty
  }
}

object BetterSetInstances extends BetterSetInstances
