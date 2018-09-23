package common.rich.func

import scala.language.higherKinds

import scalaz.{Monad, OptionT}
import scalaz.std.OptionInstances
import scalaz.syntax.ToMonadOps

trait ToMoreMonadOps extends ToMonadOps {
  implicit class toMoreMonadOps[F[_]: Monad, A]($: F[A]) {
    def toOptionTF2[B](f: A => OptionT[F, B]): OptionT[F, B] =
      OptionT($.map(Option(_))).flatMap(f)
  }
  implicit class toMoreOptionMonadOps[A, F[_]: Monad]($: F[Option[A]])
      extends ToMoreFoldableOps with OptionInstances {
    private def pure[B](e: B) = Monad.apply.pure(e)
    def mFilterOpt(p: A => F[Boolean]): F[Option[A]] = for {
      e <- $
      predValue <- e.mapHeadOrElse(p, pure(true))
      result <- if (predValue) $ else pure(None)} yield result
    def ifNoneTry(other: => F[A]): F[A] = $.flatMap(_.mapHeadOrElse(pure, other))
  }
}
