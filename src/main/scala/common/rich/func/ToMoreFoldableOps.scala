package common.rich.func

import scala.language.higherKinds
import scalaz.syntax.ToFoldableOps
import scalaz.{Foldable, MonadError, PlusEmpty}

trait ToMoreFoldableOps extends ToFoldableOps {
  implicit class toMoreFoldableOps[A, F[_] : Foldable]($: F[A]) extends ToMoreMonadErrorOps {
    def doForEach(f: A => Unit): F[A] = {
      // because scalaz isn't tail recursive 🔔 shame 🔔 shame 🔔
      var list: List[A] = Nil
      Foldable[F].any($) { e => list = e :: list; false }
      list foreach f
      $
    }
    def printPerLine(): F[A] = doForEach(println)
    // Why isn't this in the scalaz library? Who knows
    def foldMapPE[M[_] : PlusEmpty, B](f: A => M[B]): M[B] =
      Foldable[F].foldMap($)(f)(PlusEmpty[M].monoid)
    def foldLeftME[M[_], B, S](f: A => M[B], b: M[B])(implicit ev: MonadError[M, S]): M[B] =
      Foldable[F].foldLeft($, b)(_ orElseTry f(_))
    def mapHeadOrElse[B](f: A => B, default: => B): B = $.index(0).fold(default)(f)
  }
}

