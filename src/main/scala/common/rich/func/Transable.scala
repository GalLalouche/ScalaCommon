package common.rich.func

import scala.language.higherKinds

import scalaz.{Bind, Functor, IdT, ListT, OptionT}
import scalaz.Id.Id
import scalaz.Scalaz.{ToBindOps, ToFunctorOps}

trait Transable[M[_], MT[_[_], _]] {
  def hoistId[F[_] : Point, A](fa: M[A]): MT[F, A]
  def run[F[_], A](fa: MT[F, A]): F[M[A]]
  def unrun[F[_], A](fa: F[M[A]]): MT[F, A]
  def subFlatMap[F[_], A, B](fa: MT[F, A])(f: A => M[B])(implicit evF: Functor[F], evM: Bind[M]): MT[F, B] = {
    val x: F[M[A]] = run(fa)
    unrun(x.map(_.flatMap(f)))
  }
}
object Transable {
  implicit object OptionMyMonadTrans extends Transable[Option, OptionT] {
    override def hoistId[F[_] : Point, A](fa: Option[A]): OptionT[F, A] =
      OptionT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: OptionT[F, A]): F[Option[A]] = fa.run
    override def unrun[F[_], A](fa: F[Option[A]]): OptionT[F, A] = OptionT(fa)
  }
  implicit object ListMyMonadTrans extends Transable[List, ListT] {
    override def hoistId[F[_] : Point, A](fa: List[A]): ListT[F, A] =
      ListT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: ListT[F, A]): F[List[A]] = fa.run
    override def unrun[F[_], A](fa: F[List[A]]): ListT[F, A] = ListT(fa)
  }
  implicit object IdMyMonadTrans extends Transable[Id, IdT] {
    override def hoistId[F[_] : Point, A](fa: Id[A]): IdT[F, A] =
      IdT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: IdT[F, A]): F[Id[A]] = fa.run
    override def unrun[F[_], A](fa: F[Id[A]]): IdT[F, A] = IdT(fa)
  }

  def apply[M[_], MT[_[_], _]](implicit ev: Transable[M, MT]) = ev
}
