package common.rich.func.kats

import cats.Monoid

trait ToMoreMonoidOps {
  implicit class monoidFilter[A](a: => A)(implicit M: Monoid[A]) {
    def monoidFilter(b: Boolean): A = if (b) a else M.empty
    def monoidFilter(p: A => Boolean): A = if (p(a)) a else M.empty
  }
}

object ToMoreMonoidOps extends ToMoreMonoidOps
