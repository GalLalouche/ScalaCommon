package common.rich.func.kats

import cats.{Functor, FunctorFilter}
import cats.implicits.{toFunctorFilterOps, toFunctorOps}

import scala.util.Try

trait ToMoreFunctorFilterOps {
  implicit class toMoreFunctorFilterOps[A, F[_]: FunctorFilter]($ : F[A]) {
    private implicit val functor: Functor[F] = FunctorFilter[F].functor
    def tryMap[B](f: A => B): F[B] = $.mapFilter(e => Try(f(e)).toOption)
    def withFilter(p: A => Boolean): F[A] = $.filter(p)
    def toGuard(implicit ev: A =:= Boolean): F[Unit] = $.filter(ev).void
    def select[B <: A: Manifest]: F[B] = $.collect { case b: B => b }
  }
}

object ToMoreFunctorFilterOps extends ToMoreFunctorFilterOps
