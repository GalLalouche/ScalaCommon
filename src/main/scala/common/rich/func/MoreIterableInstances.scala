package common.rich.func

import scala.language.higherKinds
import scalaz.{Foldable, MonadPlus, Monoid}

// Extending IterableInstances messes up other collections
trait MoreIterableInstances {
  implicit object IterableMonadPlus extends MonadPlus[Iterable] with Foldable[Iterable] {
    override def bind[A, B](fa: Iterable[A])(f: A => Iterable[B]) = fa flatMap f
    override def point[A](a: => A) = Iterable(a)
    override def empty[A] = Iterable.empty
    override def plus[A](a: Iterable[A], b: => Iterable[A]) = a ++ b

    // Optimized implementation
    override def foldRight[A, B](fa: Iterable[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
    override def foldMap[A, B: Monoid](fa: Iterable[A])(f: A => B): B =
      fa.map(f).fold(Monoid[B].zero)(Monoid[B].append(_, _))
    override def foldLeft[A, B](fa: Iterable[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
  }
}
