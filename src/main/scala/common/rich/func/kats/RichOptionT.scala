package common.rich.func.kats

import cats.{Functor, Monad, MonadError}
import cats.data.OptionT
import cats.implicits.toFunctorOps

import scala.language.reflectiveCalls

object RichOptionT {
  implicit class richOptionT[F[_], A](private val $ : OptionT[F, A]) extends AnyVal {
    def |(default: => A)(implicit F: Functor[F]): F[A] = $.getOrElse(default)
    def |||(other: => OptionT[F, A])(implicit ev: Monad[F]): OptionT[F, A] = $.orElse(other)
    def ||||(other: => F[A])(implicit ev: Monad[F]): F[A] = $.getOrElseF(other)
    def get(implicit ev: MonadError[F, Throwable]): F[A] =
      $.getOrRaise(new NoSuchElementException)
    def getOrThrow(msg: => String)(implicit ev: MonadError[F, Throwable]): F[A] =
      $.getOrRaise(new NoSuchElementException(msg))
  }

  implicit class richFunctorToOptionT[F[_]: Functor, A]($ : F[A]) {
    def liftSome: OptionT[F, A] = OptionT($.map(Option(_)))
  }
}
