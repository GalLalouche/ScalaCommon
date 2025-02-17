package common.rich.func

import scala.language.higherKinds

import scalaz.{IList, ListT}

trait ListTInstances {
  implicit object ListMyMonadTrans extends Transable[IList, ListT] {
    override def hoistId[F[_]: Point, A](fa: IList[A]): ListT[F, A] =
      ListT(Point.apply[F].point(fa))
    override def run[F[_], A](fa: ListT[F, A]): F[IList[A]] = fa.run
    override def unrun[F[_], A](fa: F[IList[A]]): ListT[F, A] = ListT(fa)
  }
}
