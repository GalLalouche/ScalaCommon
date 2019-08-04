package common.rich.func

import scala.language.{higherKinds, reflectiveCalls}

import scalaz.{Bind, OptionT}

object ToMoreBindOps {
  import scalaz.syntax.bind._

  implicit class toMoreBindOps[F[_] : Bind, A]($: F[A]) {
    def toOptionTB[B](f: A => F[Option[B]]): OptionT[F, B] = OptionT($.flatMap(f))
  }
}
