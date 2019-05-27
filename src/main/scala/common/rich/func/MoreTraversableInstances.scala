package common.rich.func

import scala.language.higherKinds
import scalaz.{Foldable, MonadPlus, Monoid}

trait MoreTraversableInstances {
  implicit object TraversableMonadPlus extends MonadPlus[Traversable] with Foldable[Traversable] {
    override def bind[A, B](fa: Traversable[A])(f: A => Traversable[B]) = fa flatMap f
    override def point[A](a: => A) = Traversable(a)
    override def empty[A] = Traversable.empty
    override def plus[A](a: Traversable[A], b: => Traversable[A]) = a ++ b

    // Optimized implementation
    override def foldMap[A, B: Monoid](fa: Traversable[A])(f: A => B): B =
      fa.map(f).fold(Monoid[B].zero)(Monoid[B].append(_, _))
    override def foldRight[A, B](fa: Traversable[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
    override def foldLeft[A, B](fa: Traversable[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
  }
}

object MoreTraversableInstances extends MoreTraversableInstances
