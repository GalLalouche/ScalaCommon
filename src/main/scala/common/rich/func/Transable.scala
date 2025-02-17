package common.rich.func

import scala.language.higherKinds

import scalaz.{Bind, Functor, IdT, IList, ListT, OptionT}
import scalaz.Id.Id
import scalaz.Scalaz.{ToBindOps, ToFunctorOps}

/**
 * Defines the relationship between a monad transformer and the original monad, e.g., [[IList]] and
 * [[ListT]].
 */
trait Transable[M[_], MT[_[_], _]] {
  /** Like liftM, but doesn't require a Monad. */
  def hoistId[F[_]: Point, A](fa: M[A]): MT[F, A]
  /** Unwraps a monad transformer. */
  def run[F[_], A](fa: MT[F, A]): F[M[A]]
  /** Wraps a monad transformer. */
  def unrun[F[_], A](fa: F[M[A]]): MT[F, A]
  def subFlatMap[F[_], A, B](
      fa: MT[F, A],
  )(f: A => M[B])(implicit evF: Functor[F], evM: Bind[M]): MT[F, B] = {
    val x: F[M[A]] = run(fa)
    unrun(x.map(_.flatMap(f)))
  }
}
object Transable extends ListTInstances {
  implicit object OptionMyMonadTrans extends Transable[Option, OptionT] {
    override def hoistId[F[_]: Point, A](fa: Option[A]): OptionT[F, A] =
      OptionT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: OptionT[F, A]): F[Option[A]] = fa.run
    override def unrun[F[_], A](fa: F[Option[A]]): OptionT[F, A] = OptionT(fa)
  }
  implicit object IdMyMonadTrans extends Transable[Id, IdT] {
    override def hoistId[F[_]: Point, A](fa: Id[A]): IdT[F, A] =
      IdT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: IdT[F, A]): F[Id[A]] = fa.run
    override def unrun[F[_], A](fa: F[Id[A]]): IdT[F, A] = IdT(fa)
  }

  def apply[M[_], MT[_[_], _]](implicit ev: Transable[M, MT]) = ev
}
