package common.rich.func.scalazz

import scalaz.{Functor, OptionT}
import scalaz.syntax.functor.ToFunctorOps

import common.rich.RichT._

trait ToMoreFunctorOps {
  implicit class toMoreFunctorOps[F[_]: Functor, A]($ : F[A]) {
    /**
     * N.B. If `f` returns an effectful `F[_]` value, this method can't/won't actually run/wait for
     * that F to complete! (it is a Functor op, after all.) If you wish to run a proper effect while
     * returning the original value, use Bind's `>>!`
     */
    def listen(f: A => Any): F[A] = $.map(_ <| f)
    def toOptionTF[B](f: A => Option[B]): OptionT[F, B] = OptionT($.map(f))
    def unzip[B, C](implicit ev: A <:< (B, C)): (F[B], F[C]) = $.map(_._1) -> $.map(_._2)
    def ifNone[B](default: => B)(implicit ev: A <:< Option[B]): F[B] = $.map(_.getOrElse(default))
    def when[B](whenTrue: B, whenFalse: B)(implicit ev: A =:= Boolean): F[B] =
      $.map(b => if (ev(b)) whenTrue else whenFalse)
    def negated(implicit ev: A =:= Boolean): F[Boolean] = $.map(!ev(_))
  }
}

object ToMoreFunctorOps extends ToMoreFunctorOps {
  def toProduct[F[_]: Functor, A, B](f: A => F[B]): A => F[(A, B)] = a => f(a).map(a -> _)
}
