package common.rich.func

import scala.language.higherKinds

import scalaz.{Applicative, Monad, OptionT, StreamT}
import scalaz.syntax.functor.ToFunctorOps

object RichStreamT {
  implicit class richStreamT[A, F[_]](private val $: StreamT[F, A]) extends AnyVal {
    def last(implicit ev: Monad[F]): F[A] = $.toStream.map(_.last)
  }
  def iterateM[A, F[_] : Applicative](a: A)(f: A => OptionT[F, A]): StreamT[F, A] = {
    def notFirst(a: A) = (a, a -> false)
    StreamT.unfoldM((a, true)) {
      case (a, isFirst) => (if (isFirst) OptionT.some(notFirst(a)) else f(a).map(notFirst)).run
    }
  }
  def singleton[F[_] : Applicative, A](value: F[A]): StreamT[F, A] =
    StreamT.fromStream(value.map(Stream(_)))
}
