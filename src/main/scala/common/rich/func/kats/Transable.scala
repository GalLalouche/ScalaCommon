package common.rich.func.kats

import alleycats.Pure
import cats.{Functor, Id}
import cats.data.{EitherT, IdT, Ior, IorT, OptionT, WriterT}

/**
 * Defines the relationship between a monad transformer and the original monad, e.g., [[Option]] and
 * [[OptionT]].
 *
 * @tparam M
 *   The original monad, e.g., [[Option]]
 * @tparam MT
 *   The monad transformer, e.g., [[OptionT]]
 */
trait Transable[M[_], MT[_[_], _]] {
  /** Like liftF, but doesn't require a [[Functor]]. */
  def hoistId[F[_]: Pure, A](fa: M[A]): MT[F, A]
  /** Unwraps a monad transformer. */
  def value[F[_], A](fa: MT[F, A]): F[M[A]]
  /** Wraps a monad transformer. */
  def wrap[F[_], A](fa: F[M[A]]): MT[F, A]
  /**
   * An alternative to something like [[OptionT.some]] some requires [[Pure]] instead of a full
   * [[cats.Applicative]].
   */
  def pureLift[F[_], A](a: A)(implicit pureM: Pure[M], pureF: Pure[F]): MT[F, A] =
    wrap(pureF.pure(pureM.pure(a)))
}

object Transable {
  implicit object OptionTransable extends Transable[Option, OptionT] {
    override def hoistId[F[_]: Pure, A](fa: Option[A]): OptionT[F, A] = OptionT(Pure[F].pure(fa))
    override def value[F[_], A](fa: OptionT[F, A]): F[Option[A]] = fa.value
    override def wrap[F[_], A](fa: F[Option[A]]): OptionT[F, A] = OptionT(fa)
  }

  implicit object IdTransable extends Transable[Id, IdT] {
    override def hoistId[F[_]: Pure, A](fa: Id[A]): IdT[F, A] =
      IdT(Pure[F].pure(fa))
    override def value[F[_], A](fa: IdT[F, A]): F[Id[A]] = fa.value
    override def wrap[F[_], A](fa: F[Id[A]]): IdT[F, A] = IdT(fa)
  }

  implicit def eitherTransable[E]: Transable[Either[E, *], EitherT[*[_], E, *]] =
    new Transable[Either[E, *], EitherT[*[_], E, *]] {
      override def hoistId[F[_]: Pure, A](fa: E Either A): EitherT[F, E, A] =
        EitherT(Pure[F].pure(fa))
      override def value[F[_], A](fa: EitherT[F, E, A]): F[E Either A] = fa.value
      override def wrap[F[_], A](fa: F[E Either A]): EitherT[F, E, A] = EitherT(fa)
    }

  implicit def iorTransable[E]: Transable[Ior[E, *], IorT[*[_], E, *]] =
    new Transable[Ior[E, *], IorT[*[_], E, *]] {
      override def hoistId[F[_]: Pure, A](fa: E Ior A): IorT[F, E, A] =
        IorT(Pure[F].pure(fa))
      override def value[F[_], A](fa: IorT[F, E, A]): F[E Ior A] = fa.value
      override def wrap[F[_], A](fa: F[E Ior A]): IorT[F, E, A] = IorT(fa)
    }

  type WriterTuple[L, A] = (L, A)
  implicit def writerTransable[L]: Transable[WriterTuple[L, *], WriterT[*[_], L, *]] =
    new Transable[WriterTuple[L, *], WriterT[*[_], L, *]] {
      override def hoistId[F[_]: Pure, A](fa: WriterTuple[L, A]): WriterT[F, L, A] =
        WriterT(Pure[F].pure(fa))
      override def value[F[_], A](fa: WriterT[F, L, A]): F[WriterTuple[L, A]] = fa.run
      override def wrap[F[_], A](fa: F[WriterTuple[L, A]]): WriterT[F, L, A] = WriterT(fa)
    }

  // StateT is unfortunately not Transable, since the cats implementation has no equivalent of
  // 'runStateT :: StateT s m a -> m (State s a)' without any additional constraints (it would need m to be a
  // FlatMap for it to properly work). The same holds for Reader.

  def apply[M[_], MT[_[_], _]](implicit ev: Transable[M, MT]): Transable[M, MT] = ev
}
