package common.rich.func.kats

import cats.Monad

trait ToSequentialTraverseOps {
  implicit class sequentialTraverseOps[F[_]: SequentialTraverse, A]($ : F[A]) {
    def mapM[G[_]: Monad, B](f: A => G[B]): G[F[B]] = SequentialTraverse[F].mapM($)(f)
  }
}
object ToSequentialTraverseOps extends ToSequentialTraverseOps
