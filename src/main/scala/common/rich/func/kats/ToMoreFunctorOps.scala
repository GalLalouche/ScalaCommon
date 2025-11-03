package common.rich.func.kats

import cats.Functor
import cats.data.OptionT
import cats.syntax.functor.toFunctorOps

import common.rich.RichT._
import common.rich.primitives.RichBoolean.richBoolean

trait ToMoreFunctorOps {
  implicit class toMoreFunctorOps[F[_]: Functor, A]($ : F[A]) {
    /**
     * N.B. If `f` returns an effectful `F[_]` value, this method can't/won't actually run/wait for
     * that F to complete! (it is a Functor op, after all.) If you wish to run a proper effect while
     * returning the original value, use [[cats.Monad.flatTap]].
     */
    def listen(f: A => Any): F[A] = $.map(_ <| f)
    def asByName[B](b: => B): F[B] = $.map(_ => b)
    def tupleRightByName[B](b: => B): F[(A, B)] = $.map(_ -> b)
    def tupleLeftByName[B](b: => B): F[(B, A)] = $.map(b -> _)
    /** Alias to [[asByName]]. */
    def >|[B](b: => B): F[B] = $.asByName(b)
    def toOptionTF[B](f: A => Option[B]): OptionT[F, B] = OptionT($.map(f))
    def ifNone[B](default: => B)(implicit ev: A <:< Option[B]): F[B] = $.map(_.getOrElse(default))
    def negated(implicit ev: A =:= Boolean): F[Boolean] = $.map(ev(_).isFalse)
    def fpair: F[(A, A)] = $.map(e => (e, e))
  }

  implicit class toMoreFunctorPairOps[F[_]: Functor, A, B]($ : F[(A, B)]) {
    /** Note: this does *not* work on mutable (and unlawful) functors, such as [[Iterator]]. */
    def unzip: (F[A], F[B]) = Functor[F].unzip($)
  }
}

object ToMoreFunctorOps extends ToMoreFunctorOps
