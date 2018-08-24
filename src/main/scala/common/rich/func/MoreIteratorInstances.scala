package common.rich.func

import scalaz.{Foldable, MonadPlus, Monoid}

import scala.language.higherKinds

trait MoreIteratorInstances {
  implicit object IteratorMonadPlus extends MonadPlus[Iterator] with Foldable[Iterator] {
    override def bind[A, B](fa: Iterator[A])(f: A => Iterator[B]) = fa flatMap f
    override def point[A](a: => A) = Iterator(a)
    override def empty[A] = Iterator.empty
    override def plus[A](a: Iterator[A], b: => Iterator[A]) = a ++ b

    // Optimized implementation
    override def foldMap[A, B: Monoid](fa: Iterator[A])(f: A => B): B =
      fa.map(f).fold(Monoid[B].zero)(Monoid[B].append(_, _))
    override def foldRight[A, B](fa: Iterator[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
    override def foldLeft[A, B](fa: Iterator[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
  }
}
