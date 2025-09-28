package common.rich.func

import scalaz.{\/, EitherT, IList, ListT}

trait VersionDependentTransableInstances {
  implicit object ListTransable extends Transable[IList, ListT] {
    override def hoistId[F[_]: Point, A](fa: IList[A]): ListT[F, A] =
      ListT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: ListT[F, A]): F[IList[A]] = fa.run
    override def unrun[F[_], A](fa: F[IList[A]]): ListT[F, A] = ListT(fa)
  }

  implicit def eitherTransable[E]: Transable[\/[E, *], EitherT[E, *[_], *]] =
    new Transable[\/[E, *], EitherT[E, *[_], *]] {
      override def hoistId[F[_]: Point, A](fa: E \/ A): EitherT[E, F, A] =
        EitherT(Point[F].point(fa))
      override def run[F[_], A](fa: EitherT[E, F, A]): F[E \/ A] = fa.run
      override def unrun[F[_], A](fa: F[E \/ A]): EitherT[E, F, A] = EitherT(fa)
    }
}
