package common.rich.func.kats

import cats.TraverseFilter
import cats.data.State
import cats.implicits.toTraverseFilterOps

import common.rich.primitives.RichBoolean.richBoolean

trait ToMoreTraverseFilterOps {
  implicit class toMoreTraverseFilterOps[F[_]: TraverseFilter, A]($ : F[A]) {
    def uniqueBy[B](f: A => B): F[A] =
      $.filterA { a =>
        val b = f(a)
        for {
          isNew <- State.inspect[Set[B], Boolean](_(b).isFalse)
          _ <- State.modify[Set[B]](_ + b)
        } yield isNew
      }.runEmptyA.value
  }
}
object ToMoreTraverseFilterOps extends ToMoreTraverseFilterOps
