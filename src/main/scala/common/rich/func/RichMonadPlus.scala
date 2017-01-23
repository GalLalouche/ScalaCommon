package common.rich.func

import common.rich.RichT._

import scala.util.Try
import scalaz.MonadPlus
import scalaz.syntax.ToMonadPlusOps

object RichMonadPlus extends ToMonadPlusOps {
  implicit class richMonadPlus[A, F[A] : MonadPlus]($: F[A]) {
    def tryMap[B](f: A => B): F[B] =
      $.map(e => Try(f(e)))
          .filter(_.isSuccess)
          .map(_.get)
    // because flatMap only works on collections
    def oMap[B](f: A => Option[B]): F[B] = $.map(f).filter(_.isDefined).map(_.get)
    def select[B <: A : Manifest]: F[B] =
      // ToMonadOps isn't working, possible because there are two implicits in scope?
      implicitly[MonadPlus[F]].map($)(_.safeCast[B])
          .mapTo(implicitly[MonadPlus[F]].filter(_)(_.isDefined))
          .mapTo(implicitly[MonadPlus[F]].map(_)(_.get))
  }
}
