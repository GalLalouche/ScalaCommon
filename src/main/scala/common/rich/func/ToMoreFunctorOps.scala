package common.rich.func

import scala.language.higherKinds
import scalaz.Functor
import scalaz.syntax.ToFunctorOps

trait ToMoreFunctorOps extends ToFunctorOps {
  implicit class toMoreZipFunctorOps[A, B, F[_] : Functor]($: F[(A, B)]) {
    def unzip: (F[A], F[B]) = $.map(_._1) -> $.map(_._2)
  }
  implicit class toMoreFunctorOps[F[_] : Functor, A]($: F[A]) {
    def listen(f: A => Any): F[A] = $.map(e => { f(e); e })
  }
  implicit class toMoreOptFunctorOps[F[_] : Functor, A]($: F[Option[A]]) {
    def ifNone(default: => A): F[A] = $.map(_ getOrElse default)
  }
}

