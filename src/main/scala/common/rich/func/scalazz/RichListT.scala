package common.rich.func.scalazz

import scalaz.{Functor, ListT}

object RichListT {
  implicit class richListT[F[_], A](private val $ : ListT[F, A]) extends AnyVal {
    def withFilter(p: A => Boolean)(implicit ev: Functor[F]): ListT[F, A] = $.filter(p)
  }
}
