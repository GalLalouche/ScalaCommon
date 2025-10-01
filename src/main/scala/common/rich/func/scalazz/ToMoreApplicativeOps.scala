package common.rich.func.scalazz

import scalaz.Applicative
import scalaz.Scalaz.ApplicativeIdV
import scalaz.syntax.applicative.ToApplicativeOps
import scalaz.syntax.functor.ToFunctorOps

import common.rich.primitives.RichBoolean.richBoolean

trait ToMoreApplicativeOps {
  implicit class toMoreApplicativeUnitOps[F[_]: Applicative]($ : F[Unit]) {
    def withFilter(p: Unit => Boolean): F[Unit] = $.whenM(p())
  }
  // Because some applicatives are eager, e.g., Future.
  implicit class toLazyApplicativeUnitOps[F[_]: Applicative]($ : => F[_]) {
    def whenMLazy(b: Boolean): F[Unit] = if (b) $.void else ().point
    def unlessMLazy(b: Boolean): F[Unit] = whenMLazy(b.isFalse)
  }
}

object ToMoreApplicativeOps extends ToMoreApplicativeOps
