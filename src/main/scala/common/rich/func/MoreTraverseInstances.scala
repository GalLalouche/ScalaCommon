package common.rich.func

import scala.collection.generic.GenericTraversableTemplate
import scala.language.higherKinds

import scalaz.syntax.applicative._
import scalaz.{Applicative, Traverse}

object MoreTraverseInstances {
  implicit def traversableTraverse[F[X] <: Traversable[X] with GenericTraversableTemplate[X, F]]: Traverse[F] =
    new Traverse[F] {
      override def traverseImpl[G[_], A, B](fa: F[A])(f: A => G[B])(implicit app: Applicative[G]): G[F[B]] =
      // Known intellij bug: https://youtrack.jetbrains.com/issue/SCL-12929
      // I'm so going to pay for this :|
        app.map(fa./:(fa.genericBuilder[B].η)((builder, x) => (builder ⊛ f(x)) (_ += _)))(_.result)
    }
}
