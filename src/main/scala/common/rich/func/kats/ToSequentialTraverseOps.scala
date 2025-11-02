package common.rich.func.kats

import cats.Monad

trait ToSequentialTraverseOps {
  implicit class sequentialTraverseOps[F[_], A]($ : F[A])(implicit st: SequentialTraverse[F]) {
    def mapM[G[_]: Monad, B](f: A => G[B]): G[F[B]] = st.mapM($)(f)
  }
}
object ToSequentialTraverseOps extends ToSequentialTraverseOps
