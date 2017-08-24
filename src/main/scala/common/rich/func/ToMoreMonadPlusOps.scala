package common.rich.func

import common.rich.RichT._

import scala.language.higherKinds
import scala.util.Try
import scalaz.MonadPlus
import scalaz.syntax.ToMonadPlusOps

trait ToMoreMonadPlusOps extends ToMonadPlusOps {
  implicit class richMonadPlus[A, F[_] : MonadPlus]($: F[A]) {
    def tryMap[B](f: A => B): F[B] =
      $.map(e => Try(f(e)))
          .filter(_.isSuccess)
          .map(_.get)
    // because flatMap only works on collections
    def oMap[B](f: A => Option[B]): F[B] = $.map(f).filter(_.isDefined).map(_.get)
    def select[B <: A : Manifest]: F[B] = oMap(_.safeCast[B])
  }
}
