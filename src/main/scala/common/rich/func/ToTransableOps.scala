package common.rich.func

import scala.language.higherKinds

import scalaz.{Bind, Functor}

trait ToTransableOps {
  implicit class toTransableOps[MT[_[_], _], M[_], F[_], A]($: MT[F, A])(implicit evT: Transable[M, MT]) {
    def subFlatMap[B](f: A => M[B])(implicit evF: Functor[F], evM: Bind[M]): MT[F, B] = evT.subFlatMap($)(f)
  }
  implicit class toUnrunOps[MT[_[_], _], M[_], F[_], A]($: F[M[A]])(implicit evT: Transable[M, MT]) {
    def unrun: MT[F, A] = evT.unrun($)
  }
  implicit class toHoistIdOps[M[_], A]($: M[A]) {
    def hoistId[F[_] : Point, T[_[_], _]](implicit ev: Transable[M, T]): T[F, A] = ev.hoistId($)
  }
}
object ToTransableOps extends ToTransableOps
