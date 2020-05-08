package common.rich.func

import scala.language.higherKinds

import scalaz.{IdT, ListT, OptionT}
import scalaz.Id.Id

trait Transable[M[_], T[_[_], _]] {
  def hoistId[F[_] : Point, A](fa: M[A]): T[F, A]
}
object Transable {
  implicit object OptionMyMonadTrans extends Transable[Option, OptionT] {
    override def hoistId[F[_] : Point, A](fa: Option[A]): OptionT[F, A] =
      OptionT(Point.apply[F].point(fa))
  }
  implicit object ListMyMonadTrans extends Transable[List, ListT] {
    override def hoistId[F[_] : Point, A](fa: List[A]): ListT[F, A] =
      ListT(Point.apply[F].point(fa))
  }
  implicit object IdMyMonadTrans extends Transable[Id, IdT] {
    override def hoistId[F[_] : Point, A](fa: Id[A]): IdT[F, A] =
      IdT(Point.apply[F].point(fa))
  }
}
