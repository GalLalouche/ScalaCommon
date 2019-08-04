package common.rich.func

import scala.language.higherKinds

import scalaz.{Applicative, MonadPlus, Traverse}
import scalaz.syntax.traverse._

object MoreTraverseMonadPlusSyntax {
  import scalaz.syntax.monadPlus._

  implicit class moreTraverseMonadPlusSyntax[F[_] : MonadPlus : Traverse, A](f: F[A]) {
    import scalaz.syntax.applicative._

    def filterTraverse[G[_] : Applicative](p: A => G[Boolean]): G[F[A]] =
      f.traverse(e => p(e).map(e -> _)).map(_.filter(_._2).tmap(_._1))
  }
}
