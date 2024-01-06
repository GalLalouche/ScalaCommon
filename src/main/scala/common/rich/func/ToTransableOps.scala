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
}
object ToTransableOps extends ToTransableOps
