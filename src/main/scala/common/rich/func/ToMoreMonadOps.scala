package common.rich.func

import scala.language.higherKinds

import scalaz.Monad
import scalaz.syntax.applicative.ApplicativeIdV
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.functor.ToFunctorOps

trait ToMoreMonadOps {
  implicit class toMonadBooleanOps[F[_] : Monad, A]($: F[Boolean]) {
    def ifFalse(f: => F[_]): F[Unit] = $.ifM(ifTrue = ().pure, ifFalse = f.void)
    def ifTrue(f: => F[_]): F[Unit] = $.ifM(ifTrue = f.void, ifFalse = ().pure)
  }
}

object ToMoreMonadOps extends ToMoreMonadOps
