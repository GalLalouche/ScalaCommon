package common.rich.func

import scala.language.higherKinds
import scalaz.Functor
import scalaz.syntax.ToFunctorOps

trait ToMoreFunctorOps extends ToFunctorOps {
  implicit class richZipFunctor[A, B, F[_] : Functor]($: F[(A, B)]) {
    def unzip: (F[A], F[B]) = $.map(_._1) -> $.map(_._2)
  }
}

