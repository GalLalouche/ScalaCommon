package common.rich.func

import scala.collection.generic.GenericTraversableTemplate
import scala.languageFeature.higherKinds

import scalaz.{Applicative, Traverse}
import scalaz.syntax.ToApplicativeOps

trait MoreTraverseInstances {
  implicit def traversableTraverse[F[X] <: Traversable[X] with GenericTraversableTemplate[X, F]]: Traverse[F] =
  // Use traits since this doesn't compile in Intellij.
    new Traverse[F] with ToApplicativeOps {
      override def traverseImpl[G[_], A, B](fa: F[A])(f: A => G[B])(implicit app: Applicative[G]): G[F[B]] =
      // Known intellij bug: https://youtrack.jetbrains.com/issue/SCL-12929
      // I'm so going to pay for this :|
        fa./:(fa.genericBuilder[B].η)((builder, x) => (builder ⊛ f(x)) (_ += _)) ∘ (_.result)
    }
}

object MoreTraverseInstances extends MoreTraverseInstances
