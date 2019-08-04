package common.rich.func

import scala.language.higherKinds

import scalaz.{Functor, OptionT}

import common.rich.RichT._

object ToMoreFunctorOps {
  import scalaz.syntax.functor._

  implicit class toMoreFunctorOps[F[_] : Functor, A]($: F[A]) {
    def listen(f: A => Any): F[A] = $.map(_ <| f)
    def toOptionTF[B](f: A => Option[B]): OptionT[F, B] = OptionT($.map(f))
    def unzip[B, C](implicit ev: A <:< (B, C)): (F[B], F[C]) = $.map(_._1) -> $.map(_._2)
    def ifNone[B](default: => B)(implicit ev: A <:< Option[B]): F[B] = $.map(_ getOrElse default)
    def when[B](whenTrue: B, whenFalse: B)(implicit ev: A =:= Boolean): F[B] =
      $.map(b => if (ev(b)) whenTrue else whenFalse)
  }
}
