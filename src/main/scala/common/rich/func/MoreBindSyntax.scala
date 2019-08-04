package common.rich.func

import scala.language.{higherKinds, reflectiveCalls}

import scalaz.{Bind, OptionT}

object MoreBindSyntax {
  import scalaz.syntax.bind._

  implicit class moreBindSyntax[F[_] : Bind, A]($: F[A]) {
    def toOptionTB[B](f: A => F[Option[B]]): OptionT[F, B] = OptionT($.flatMap(f))
  }
}
