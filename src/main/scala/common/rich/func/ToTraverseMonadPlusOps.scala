package common.rich.func

import common.rich.func.ToMoreMonadPlusOps._
import scalaz.syntax.traverse._
import scalaz.{Applicative, MonadPlus, Traverse}

import scala.language.higherKinds

trait ToTraverseMonadPlusOps {
  implicit class toTraverseMonadPlusOps[F[_] : MonadPlus : Traverse, A](f: F[A]) {
    import scalaz.syntax.applicative._

    def filterTraverse[G[_] : Applicative](p: A => G[Boolean]): G[F[A]] =
      f.traverse(e => p(e).map(e -> _)).map(_.filter(_._2).tmap(_._1))
  }
}

object ToTraverseMonadPlusOps extends ToTraverseMonadPlusOps
