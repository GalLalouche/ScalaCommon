package common.rich.func

import scalaz.Applicative
import scalaz.syntax.ToApplicativeOps

import scala.language.higherKinds

trait ToMoreApplicativeOps extends ToApplicativeOps {
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
