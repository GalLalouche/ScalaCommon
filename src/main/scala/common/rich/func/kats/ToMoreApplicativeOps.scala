package common.rich.func.kats

import cats.Applicative
import cats.syntax.applicative.catsSyntaxApplicativeByName

trait ToMoreApplicativeOps {
  implicit class toMoreApplicativeUnitOps[F[_]: Applicative]($ : F[Unit]) {
    def withFilter(p: Unit => Boolean): F[Unit] = $.whenA(p())
  }
}

object ToMoreApplicativeOps extends ToMoreApplicativeOps
