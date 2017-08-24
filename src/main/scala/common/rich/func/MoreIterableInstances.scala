package common.rich.func

import scala.language.higherKinds
import scalaz.syntax.ToApplicativeOps
import scalaz.{Applicative, MonadPlus, Monoid, Traverse}

// Extending InterableInstances messes up other collections
trait MoreIterableInstances {
  implicit object IterableMonadPlus extends MonadPlus[Iterable] {
    override def bind[A, B](fa: Iterable[A])(f: A => Iterable[B]) = fa flatMap f
    override def point[A](a: => A) = Iterable(a)
    override def empty[A] = Iterable.empty
    override def plus[A](a: Iterable[A], b: => Iterable[A]) = a ++ b
  }
  // TODO handle code duplication
  implicit object IterableTraverse extends Traverse[Iterable] with ToApplicativeOps {
    override def traverseImpl[G[_] : Applicative, A, B](fa: Iterable[A])(f: A => G[B]): G[Iterable[B]] = {
      fa.foldLeft(Applicative[G].point(fa.genericBuilder[B])) {
        (builder, element) => Applicative[G].apply2(builder, f(element))(_ += _)
      }.map(_.result)
    }
    // Optimized implementation
    override def foldRight[A, B](fa: Iterable[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
    override def foldMap[A, B: Monoid](fa: Iterable[A])(f: A => B): B =
      fa.map(f).fold(Monoid[B].zero)(Monoid[B].append(_, _))
    override def foldLeft[A, B](fa: Iterable[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
  }
}
