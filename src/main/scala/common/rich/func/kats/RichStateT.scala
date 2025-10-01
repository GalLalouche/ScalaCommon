package common.rich.func.kats

import alleycats.Empty
import cats.FlatMap
import cats.data.StateT
import cats.implicits.toFunctorOps

object RichStateT {
  implicit class richStateT[F[_]: FlatMap, S, A](private val $ : StateT[F, S, A]) {
    implicit def runNow(initial: S): F[(S, A)] = $.run(initial)
    implicit def runEmpty(implicit M: Empty[S]): F[(S, A)] = runNow(M.empty)
    implicit def exec(initial: S): F[S] = $.run(initial).map(_._1)
    implicit def execEmpty(implicit M: Empty[S]): F[S] = exec(M.empty)
    implicit def eval(initial: S): F[A] = $.run(initial).map(_._2)
    implicit def evalEmpty(implicit M: Empty[S]): F[A] = eval(M.empty)
  }
}
