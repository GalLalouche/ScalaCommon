package common.rich.func

import scala.language.higherKinds

import common.rich.func.MonocleScalazConversions._
import monocle.Iso
import scalaz.InvariantFunctor

trait ToMoreInvariantFunctorOps {
  implicit class toMoreInvariantFunctorOps[F[_]: InvariantFunctor, A]($ : F[A]) {
    def xmapmi[B](iso: Iso[A, B]): F[B] = InvariantFunctor[F].xmapi($)(iso.toScalaz)
  }
}
object ToMoreInvariantFunctorOps extends ToMoreInvariantFunctorOps
