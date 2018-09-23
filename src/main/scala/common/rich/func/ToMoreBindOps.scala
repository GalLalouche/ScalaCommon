package common.rich.func

import scala.language.{higherKinds, reflectiveCalls}

import scalaz.{Bind, OptionT}
import scalaz.syntax.ToBindOps

trait ToMoreBindOps extends ToBindOps {
  implicit class toMoreBindOps[F[_] : Bind, A]($: F[A]) {
    def toOptionTB[B](f: A => F[Option[B]]): OptionT[F, B] = OptionT($.flatMap(f))
  }
}
