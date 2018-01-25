package common.rich.func

import scala.language.higherKinds
import scalaz.syntax.ToTraverseOps
import scalaz.{Applicative, MonadPlus, Traverse}

trait ToTraverseMonadPlusOps extends ToTraverseOps with ToMoreMonadPlusOps {
  implicit class toTraverseMonadPlusOps[F[_]: MonadPlus: Traverse, A](f: F[A]) {
    def filterTraverse[G[_]: Applicative](p: A => G[Boolean]): G[F[A]] =
      f.traverse(e => p(e).map(e -> _)).map(MonadPlus[F].filter(_)(_._2).tmap(_._1))
  }
}
