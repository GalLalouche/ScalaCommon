package common.rich.func

import scala.language.higherKinds

trait ToMoreMonadTransOps {
  implicit class toMoreMonadTransOps[M[_], A]($: M[A]) {
    def hoistId[F[_] : Point, T[_[_], _]](implicit ev: Transable[M, T]): T[F, A] = ev.hoistId($)
  }
}
object ToMoreMonadTransOps extends ToMoreMonadTransOps
