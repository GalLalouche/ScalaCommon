package common.rich.func.scalazz

import scala.language.higherKinds

import scalaz.{Foldable, MonadPlus, Monoid}
import scalaz.std.SetInstances

trait MoreSetInstances extends SetInstances {
  implicit object SetMonadPlus extends MonadPlus[Set] /*Kinda*/ with Foldable[Set] {
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
  }
}

object MoreSetInstances extends MoreSetInstances
