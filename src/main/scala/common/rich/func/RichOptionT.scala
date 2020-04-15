package common.rich.func

import scala.language.{higherKinds, reflectiveCalls}

import scalaz.{~>, Functor, Monad, MonadError, OptionT}
import scalaz.Id.Id
import scalaz.syntax.applicative.ApplicativeIdV
import scalaz.syntax.functor.ToFunctorOps
import scalaz.syntax.monad.ToMonadOps

import common.rich.primitives.RichBoolean._

object RichOptionT {
  implicit class richOptionT[F[_], A](private val $: OptionT[F, A]) extends AnyVal {
    def subFlatMap[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
      OptionT(F.map($.run)(_ flatMap f))
    def ||||(other: => F[A])(implicit ev: Monad[F]): F[A] = $ getOrElseF other
    def orError[S](s: => S)(implicit ev: MonadError[F, S]): F[A] = ev.bind($.run) {
      case Some(value) => Monad[F].point(value)
      case None => ev.raiseError(s)
    }
    def get(implicit ev: MonadError[F, Throwable]): F[A] = orError[Throwable](new NoSuchElementException)
  }

  def app[F[_] : Point]: Option ~> OptionT[F, *] = new (Option ~> OptionT[F, *]) {
    override def apply[A](fa: Option[A]) = OptionT(Point[F].point(fa))
  }
  // An alternative to OptionT.some that requires a point rather than a full Applicative.
  def pointSome[F[_] : Point]: Id ~> OptionT[F, *] = new (Id ~> OptionT[F, *]) {
    override def apply[A](a: A) = OptionT(Point[F].point(Some(a)))
  }

  def when[F[_] : Monad, A](b: Boolean)(a: => F[A]): OptionT[F, A] = whenM(b.pure)(a)
  def whenM[F[_] : Monad, A](bm: F[Boolean])(a: => F[A]): OptionT[F, A] = for {
    b <- bm.liftM[OptionT]
    result <- if (b) a.liftM[OptionT] else OptionT.none
  } yield result
  def unless[F[_] : Monad, A](b: Boolean)(a: => F[A]): OptionT[F, A] = when(b.isFalse)(a)
  def unlessM[F[_] : Monad, A](bm: F[Boolean])(a: => F[A]): OptionT[F, A] =
    whenM(bm.map(_.isFalse))(a)
  implicit class richFunctorToOptionT[F[_] : Functor, A]($: F[A]) {
    def liftSome: OptionT[F, A] = OptionT($.map(Option(_)))
  }
}
