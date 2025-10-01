package common.rich.func.scalazz

import scala.collection.IterableFactoryDefaults
import scala.languageFeature.higherKinds

import scalaz.{Applicative, Traverse}
import scalaz.Scalaz.{ApplicativeIdV, ToApplyOpsUnapply, ToFunctorOps}

trait MoreTraverseInstances {
  implicit def traversableTraverse[F[X] <: IterableFactoryDefaults[X, F]]: Traverse[F] =
    // Use traits since this doesn't compile in Intellij.
    new Traverse[F] {
      override def traverseImpl[G[_], A, B](fa: F[A])(f: A => G[B])(implicit
          app: Applicative[G],
      ): G[F[B]] =
        // Known intellij bug: https://youtrack.jetbrains.com/issue/SCL-12929
        // I'm so going to pay for this :|
        fa.foldLeft(fa.iterableFactory.newBuilder[B].η)((builder, x) =>
          (builder ⊛ f(x))(_ += _),
        ) ∘ (_.result)
      override def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
      override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B =
        fa.foldRight(z)((a, b) => f(a, b))
    }
}

object MoreTraverseInstances extends MoreTraverseInstances
