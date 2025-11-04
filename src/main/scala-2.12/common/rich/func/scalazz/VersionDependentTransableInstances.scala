package common.rich.func.scalazz

import scalaz.{\/, EitherT, ListT}

trait VersionDependentTransableInstances {
  implicit object ListTransable extends Transable[List, ListT] {
    override def hoistId[F[_]: Point, A](fa: List[A]): ListT[F, A] =
      ListT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: ListT[F, A]): F[List[A]] = fa.run
    override def unrun[F[_], A](fa: F[List[A]]): ListT[F, A] = ListT(fa)
  }

  // This is needed for the type inference to work correctly! Probably why they changed it in 7.8 ¯\_(ツ)_/¯.
  // For more information, see https://gist.github.com/djspiewak/7a81a395c461fd3a09a6941d4cd040f2,
  // https://eed3si9n.com/herding-cats/partial-unification.html, https://github.com/scalaz/scalaz/issues/1305
  type SwappedEitherT[E, F[_], A] = EitherT[F, E, A]
  def swappedEitherT[E, F[_], A](disjuction: F[E \/ A]): SwappedEitherT[E, F, A] =
    EitherT(disjuction)
  implicit def eitherTransable[E]: Transable[\/[E, *], SwappedEitherT[E, *[_], *]] =
    new Transable[\/[E, *], SwappedEitherT[E, *[_], *]] {
      override def hoistId[F[_]: Point, A](fa: E \/ A): SwappedEitherT[E, F, A] =
        EitherT(Point[F].point(fa))
      override def run[F[_], A](fa: SwappedEitherT[E, F, A]): F[E \/ A] = fa.run
      override def unrun[F[_], A](fa: F[E \/ A]): SwappedEitherT[E, F, A] = EitherT(fa)
    }
}
