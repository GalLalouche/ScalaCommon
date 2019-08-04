package common.rich.func

import scala.language.higherKinds

import scalaz.Applicative

object ToMoreApplicativeOps {
  import scalaz.syntax.applicative._

  implicit class toMoreApplicativeUnitOps[F[_] : Applicative]($: F[Unit]) {
    /**
     *  Enabling if X in for comprehensions, e.g.,
     *  for {
     *    x <- foo
     *    y <- bar(x) if bazz
     *  } yield y
     */
    def withFilter(p: Unit => Boolean): F[Unit] = $ whenM p()
  }
}
