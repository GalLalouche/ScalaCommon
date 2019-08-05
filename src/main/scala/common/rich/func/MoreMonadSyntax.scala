package common.rich.func

import scala.language.higherKinds

import scalaz.{Monad, OptionT}

object MoreMonadSyntax {
  import scalaz.syntax.monad._
  import scalaz.===

  implicit class moreMonadSyntax[F[_] : Monad, A]($: F[A]) {
    private def pure[B](e: B) = Monad.apply.pure(e)
    def toOptionTF2[B](f: A => OptionT[F, B]): OptionT[F, B] =
      OptionT($.map(Option(_))).flatMap(f)
    def ifFalse(f: F[_])(implicit ev: A === Boolean): F[Unit] = $.ifM(ifTrue = pure(()), ifFalse = f.void)
    def ifTrue(f: F[_])(implicit ev: A === Boolean): F[Unit] = $.ifM(ifTrue = f.void, ifFalse = pure(()))
  }

  //TODO move below
  implicit class moreMonadOptionSyntax[A, F[_] : Monad]($: F[Option[A]]) {
    import scalaz.std.option.optionInstance
    import MoreFoldableSyntax._

    private def pure[B](e: B): F[B] = implicitly[Monad[F]].pure(e)
    def mFilterOpt(p: A => F[Boolean]): F[Option[A]] = for {
      e <- $
      predValue <- e.mapHeadOrElse(p, pure(true))
      result <- if (predValue) $ else pure(None)
    } yield result
    def ifNoneTry(other: => F[A]): F[A] = $.flatMap(_.mapHeadOrElse(pure, other))
  }
}
