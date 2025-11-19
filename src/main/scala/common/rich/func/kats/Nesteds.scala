package common.rich.func.kats

import cats.data.Nested

object Nesteds {
  type SeqT[F[_], A] = Nested[F, Seq, A]
  def SeqT[F[_], A](fa: F[Seq[A]]): SeqT[F, A] = Nested.apply(fa)
}
