package common.rich.func

import scala.language.higherKinds

import scalaz.Applicative

object MoreApplicativeSyntax {
  import scalaz.syntax.applicative._

  implicit class moreApplicativeUnitSyntax[F[_] : Applicative]($: F[Unit]) {
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
