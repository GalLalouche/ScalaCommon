package common.rich.func

import scalaz.syntax.ToApplicativeOps
import scalaz.{Applicative, Traverse}

object MoreTraverse {
  implicit object TraversableTraverse extends Traverse[Traversable] with ToApplicativeOps {
    override def traverseImpl[G[_], A, B](fa: Traversable[A])(f: (A) => G[B])(implicit evidence: Applicative[G]): G[Traversable[B]] =
      fa.foldLeft(evidence.point(fa.genericBuilder[B])) {
        (builder, element) => evidence.apply2(builder, f(element))(_ += _)
      }.map(_.result)
  }
}
