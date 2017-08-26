package common.rich.func

import scalaz.{Applicative, MonadPlus, Traverse}
import scalaz.syntax.ToTraverseOps

trait ToTraverseMonadPlusOps extends ToTraverseOps with ToMoreMonadPlusOps {
  implicit class richTraverseMonadPlusOps[F[_]: MonadPlus: Traverse, A](f: F[A]) {
    def filterTraverse[G[_]: Applicative](p: A => G[Boolean]): G[F[A]] =
      f.traverse(e => p(e).map(e -> _)).map(MonadPlus[F].filter(_)(_._2).tmap(_._1))
  }
}
