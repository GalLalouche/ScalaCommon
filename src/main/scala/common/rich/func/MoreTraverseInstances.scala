package common.rich.func

import scala.collection.mutable
import scala.language.higherKinds

import scalaz.{Applicative, Traverse}
import scalaz.syntax.applicative._

import common.rich.collections.GenericFactory

object MoreTraverseInstances {
  implicit def iterableTraverse[F[X] <: Iterable[X]](implicit factory: GenericFactory[F]): Traverse[F] =
    new Traverse[F] {
      override def traverseImpl[G[_], A, B](fa: F[A])(f: A => G[B])(implicit app: Applicative[G]): G[F[B]] = {
        app.map(fa.foldLeft(factory.newBuilder[B].η)((builder, x) => (builder ⊛ f(x)) (_ += _)))(_.result)
      }
      // Known intellij bug: https://youtrack.jetbrains.com/issue/SCL-12929
      // I'm so going to pay for this :|
    }
}
