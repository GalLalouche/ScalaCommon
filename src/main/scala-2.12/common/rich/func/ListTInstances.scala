package common.rich.func

import scala.language.higherKinds

import scalaz.ListT

trait ListTInstances {
  implicit object ListMyMonadTrans extends Transable[List, ListT] {
    override def hoistId[F[_]: Point, A](fa: List[A]): ListT[F, A] =
      ListT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: ListT[F, A]): F[List[A]] = fa.run
    override def unrun[F[_], A](fa: F[List[A]]): ListT[F, A] = ListT(fa)
  }
}
