package common.rich.func

import scala.language.higherKinds

import scalaz.Monad
import scalaz.syntax.applicative.ApplicativeIdV
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.functor.ToFunctorOps

trait ToMoreMonadOps {
  implicit class toMonadBooleanOps[F[_] : Monad, A]($: F[Boolean]) {
    def ifFalse(f: => F[_]): F[Unit] = $.ifM(ifTrue = ().pure, ifFalse = f.void)
    def ifTrue(f: => F[_]): F[Unit] = $.ifM(ifTrue = f.void, ifFalse = ().pure)
  }

  //TODO move below
  implicit class toMoreOptionMonadOps[A, F[_] : Monad]($: F[Option[A]]) {
    import scalaz.std.option.optionInstance
    import ToMoreFoldableOps._

    private def pure[B](e: B): F[B] = implicitly[Monad[F]].pure(e)
    def mFilterOpt(p: A => F[Boolean]): F[Option[A]] = for {
      e <- $
      predValue <- e.mapHeadOrElse(p, pure(true))
      result <- if (predValue) $ else pure(None)} yield result
    def ifNoneTry(other: => F[A]): F[A] = $.flatMap(_.mapHeadOrElse(pure, other))
  }
}

object ToMoreMonadOps extends ToMoreMonadOps
