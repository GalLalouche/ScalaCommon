package common.rich.func.kats

import alleycats.Empty
import cats.data.State

object RichState {
  implicit class richState[S, A](private val $ : State[S, A]) extends AnyVal {
    implicit def runNow(initial: S): (S, A) = $.run(initial).value
    implicit def runEmpty(implicit M: Empty[S]): (S, A) = runNow(M.empty)
    implicit def exec(initial: S): S = $.run(initial).map(_._1).value
    implicit def execEmpty(implicit M: Empty[S]): S = exec(M.empty)
    implicit def eval(initial: S): A = $.run(initial).map(_._2).value
    implicit def evalEmpty(implicit M: Empty[S]): A = eval(M.empty)
  }
}
