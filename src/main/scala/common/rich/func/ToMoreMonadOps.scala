package common.rich.func

import scala.language.higherKinds
import scalaz.Monad
import scalaz.syntax.ToMonadOps

trait ToMoreMonadOps extends ToMonadOps {
  implicit class toMoreOptionMonadPlusOps[A, F[_]: Monad]($: F[Option[A]]) {
    private def pure[B](e: B) = Monad.apply.pure(e)
    def mFilterOpt(p: A => F[Boolean]): F[Option[A]] = for {
      e <- $
      predValue <- e.fold(pure(true))(p)
      result <- if (predValue) $ else pure(None)} yield result
  }
}
