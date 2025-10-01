package common.rich.func.kats

import cats.Invariant

import monocle.Iso

trait ToMoreInvariantOps {
  implicit class toMoreInvariantOps[F[_]: Invariant, A]($ : F[A]) {
    def isoMap[B](iso: Iso[A, B]): F[B] = Invariant[F].imap($)(iso.get)(iso.reverseGet)
  }
}
object ToMoreInvariantOps extends ToMoreInvariantOps
