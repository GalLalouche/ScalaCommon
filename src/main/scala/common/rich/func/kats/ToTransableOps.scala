package common.rich.func.kats

import alleycats.Pure

trait ToTransableOps {
  /** E.g., convert an [[Option]] to an [[cats.data.OptionT]]. */
  implicit class toHoistIdOps[M[_], A]($ : M[A]) {
    def hoistId[F[_]: Pure, MT[_[_], _]](implicit ev: Transable[M, MT]): MT[F, A] = ev.hoistId($)
  }
  implicit class toPureLiftOps[A]($ : A) {
    def pureLift[F[_]: Pure, M[_]: Pure, MT[_[_], _]](implicit ev: Transable[M, MT]): MT[M, A] =
      ev.pureLift($)
  }
  implicit class toWrapOps[MT[_[_], _], M[_], F[_], A]($ : F[M[A]])(implicit
      evT: Transable[M, MT],
  ) {
    def wrap: MT[F, A] = evT.wrap($)
  }
}
object ToTransableOps extends ToTransableOps
