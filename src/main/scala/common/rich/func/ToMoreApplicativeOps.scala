package common.rich.func

import scala.language.higherKinds

import scalaz.Applicative
import scalaz.syntax.applicative.ToApplicativeOps

trait ToMoreApplicativeOps {
  implicit class toMoreApplicativeUnitOps[F[_] : Applicative]($: F[Unit]) {
    def withFilter(p: Unit => Boolean): F[Unit] = $ whenM p()
  }
}

object ToMoreApplicativeOps extends ToMoreApplicativeOps
