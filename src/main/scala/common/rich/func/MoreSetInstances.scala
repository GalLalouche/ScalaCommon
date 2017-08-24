package common.rich.func

import scala.language.higherKinds
import scalaz.std.SetInstances
import scalaz.syntax.ToApplicativeOps
import scalaz.{Applicative, MonadPlus, Monoid, Traverse}

trait MoreSetInstances extends SetInstances {
  implicit object SetMonadPlus extends MonadPlus[Set] { // Kinda
    override def bind[A, B](fa: Set[A])(f: A => Set[B]) = fa flatMap f
    override def point[A](a: => A) = Set(a)
    override def empty[A] = Set.empty
    override def plus[A](a: Set[A], b: => Set[A]) = a ++ b
  }
  // TODO handle code duplication
  implicit object SetTraverse extends Traverse[Set] with ToApplicativeOps {
    override def traverseImpl[G[_]: Applicative, A, B](fa: Set[A])(f: A => G[B]): G[Set[B]] = {
      fa.foldLeft(Applicative[G].point(fa.genericBuilder[B])) {
        (builder, element) => Applicative[G].apply2(builder, f(element))(_ += _)
      }.map(_.result)
    }
    // Optimized implementation
    override def foldRight[A, B](fa: Set[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
    override def foldMap[A, B: Monoid](fa: Set[A])(f: A => B): B =
      fa.map(f).fold(Monoid[B].zero)(Monoid[B].append(_, _))
    override def foldLeft[A, B](fa: Set[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
  }
}
