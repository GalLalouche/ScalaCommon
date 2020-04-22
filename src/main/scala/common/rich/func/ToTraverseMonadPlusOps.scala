package common.rich.func

import scala.language.higherKinds

import scalaz.{Applicative, MonadPlus, Traverse}
import scalaz.syntax.functor.ToFunctorOps
import scalaz.syntax.monadPlus.ToMonadPlusOps
import scalaz.syntax.traverse.ToTraverseOps

trait ToTraverseMonadPlusOps {
  implicit class toTraverseMonadPlusOps[F[_] : MonadPlus : Traverse, A](f: F[A]) {
    // Scalaz only provides functions for List and Vector for some reason.
    def filterM[G[_] : Applicative](p: A => G[Boolean]): G[F[A]] =
      f.traverse(e => p(e).map(e -> _)).map(_.filter(_._2).tmap(_._1))
  }
}

object ToTraverseMonadPlusOps extends ToTraverseMonadPlusOps
