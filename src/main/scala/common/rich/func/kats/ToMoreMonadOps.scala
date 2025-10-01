package common.rich.func.kats

import cats.Monad
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxIfM}
import cats.syntax.functor.toFunctorOps

trait ToMoreMonadOps {
  implicit class toMonadBooleanOps[F[_]: Monad]($ : F[Boolean]) {
    def ifFalse(f: => F[_]): F[Unit] = $.ifM(ifTrue = ().pure, ifFalse = f.void)
    def ifTrue(f: => F[_]): F[Unit] = $.ifM(ifTrue = f.void, ifFalse = ().pure)
  }
}

object ToMoreMonadOps extends ToMoreMonadOps
