package common.rich.func

import scalaz.syntax.ToApplicativeOps
import scalaz.{Applicative, Traverse}

object MoreTraverse extends ToApplicativeOps {
  // TODO remove code duplication with CanBuildFrom
  implicit object TraversableTraverse extends Traverse[Traversable] {
    override def traverseImpl[G[_]: Applicative, A, B](fa: Traversable[A])(f: A => G[B]): G[Traversable[B]] =
      fa.foldLeft(implicitly[Applicative[G]].point(fa.genericBuilder[B])) {
        (builder, element) => implicitly[Applicative[G]].apply2(builder, f(element))(_ += _)
      }.map(_.result)
  }
  implicit object SeqTraverse extends Traverse[Seq] {
    override def traverseImpl[G[_]: Applicative, A, B](fa: Seq[A])(f: A => G[B]) = {
      fa.foldLeft(implicitly[Applicative[G]].point(fa.genericBuilder[B])) {
        (builder, element) => implicitly[Applicative[G]].apply2(builder, f(element))(_ += _)
      }.map(_.result)
    }
  }
}
