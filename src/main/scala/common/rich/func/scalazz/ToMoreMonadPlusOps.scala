package common.rich.func.scalazz

import scala.util.Try

import scalaz.MonadPlus
import scalaz.syntax.functor.ToFunctorOps
import scalaz.syntax.monadPlus.ToMonadPlusOps

import common.rich.RichT._

trait ToMoreMonadPlusOps {
  implicit class toMoreMonadPlusOps[A, F[_]: MonadPlus]($ : F[A]) {
    def filterNot(f: A => Boolean): F[A] = $.filter(f.andThen(!_))
    def tryMap[B](f: A => B): F[B] = $.map(e => Try(f(e)))
      .filter(_.isSuccess)
      .map(_.get)
    // because flatMap only works on collections
    def oMap[B](f: A => Option[B]): F[B] = $.map(f).filter(_.isDefined).map(_.get)
    def present[B](implicit ev: A <:< Option[B]): F[B] = oMap(ev.apply)
    def select[B <: A: Manifest]: F[B] = oMap(_.safeCast[B])
    def toGuard(implicit ev: A =:= Boolean): F[Unit] = $.filter(ev).void
    def withFilter(p: A => Boolean): F[A] = $.filter(p)
  }
}

object ToMoreMonadPlusOps extends ToMoreMonadPlusOps {
  def void[F[_]: MonadPlus]: F[Unit] = implicitly[MonadPlus[F]].empty.void
}
