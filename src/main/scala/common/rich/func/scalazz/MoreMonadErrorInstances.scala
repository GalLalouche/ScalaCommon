package common.rich.func.scalazz

import scalaz.{Monad, MonadError, OptionT}
import scalaz.std.OptionInstances

trait MoreMonadErrorInstances {
  implicit object OptionMonadError extends MonadError[Option, Unit] with OptionInstances {
    override def raiseError[A](e: Unit): Option[A] = None
    override def handleError[A](
        fa: Option[A],
    )(f: Unit => Option[A]): Option[A] = fa match {
      case None => f(())
      case e => e
    }

    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
    override def point[A](a: => A): Option[A] = Some(a)
  }

  implicit def optionTMonadError[F[_]: Monad]: MonadError[OptionT[F, *], Unit] =
    new MonadError[OptionT[F, *], Unit] {
      override def raiseError[A](e: Unit) = OptionT.none[F, A]
      override def handleError[A](fa: OptionT[F, A])(f: Unit => OptionT[F, A]) =
        fa.orElse(f(()))
      override def bind[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]) =
        fa.flatMap(f)
      override def point[A](a: => A) = OptionT.some[F, A](a)
    }
}
