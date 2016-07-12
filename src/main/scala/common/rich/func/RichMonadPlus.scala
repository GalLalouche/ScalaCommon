package common.rich.func

import scala.util.Try
import scalaz.MonadPlus
import common.rich.RichT._

object RichMonadPlus {
  implicit class richMonadPlus[A, F[A] : MonadPlus]($: F[A]) {
    def tryMap[B](f: A => B): F[B] =
      implicitly[MonadPlus[F]].map($)(e => Try(f(e)))
        .mapTo(e => implicitly[MonadPlus[F]].filter(e)(_.isSuccess))
        .mapTo(e => implicitly[MonadPlus[F]].map(e)(_.get))
  }
}
