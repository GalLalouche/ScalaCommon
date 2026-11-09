package common.rich.func.kats

import alleycats.Empty

trait ToMoreEmptyOps {
  implicit class emptyFilter[A](a: => A)(implicit M: Empty[A]) {
    def emptyFilter(b: Boolean): A = if (b) a else M.empty
    def emptyFilter(p: A => Boolean): A = if (p(a)) a else M.empty
  }
}

object ToMoreEmptyOps extends ToMoreEmptyOps {
  implicit def emptyOption[A]: Empty[Option[A]] = new Empty[Option[A]] {
    override def empty: Option[Nothing] = None
  }
}
