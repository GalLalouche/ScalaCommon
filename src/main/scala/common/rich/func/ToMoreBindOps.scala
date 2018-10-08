package common.rich.func

import scala.language.{higherKinds, reflectiveCalls}

import scalaz.{Bind, OptionT}
import scalaz.syntax.ToBindOps

trait ToMoreBindOps extends ToBindOps {
  import scalaz.Leibniz.===

  implicit class toMoreBindOps[F[_] : Bind, A]($: F[A]) {
    def toOptionTB[B](f: A => F[Option[B]]): OptionT[F, B] = OptionT($.flatMap(f))
    def ifFalse(f: F[Unit])(implicit ev: A === Boolean): F[A] = $.ifM(ifTrue = $, ifFalse = f >> $)
    def ifTrue(f: F[Unit])(implicit ev: A === Boolean): F[A] = $.ifM(ifTrue = f >> $, ifFalse = $)
  }
}
