package common.rich.func

import scala.language.higherKinds

import scalaz.{~>, Applicative, Hoist, Monad, OptionT, StreamT}
import scalaz.syntax.functor.ToFunctorOps
import scalaz.Id.Id

object RichStreamT {
  implicit class richStreamT[A, F[_]](private val $: StreamT[F, A]) extends AnyVal {
    def last(implicit ev: Monad[F]): F[A] = $.toStream.map(_.last)
    def oMapM[B](f: A => OptionT[F, B])(implicit ev: Applicative[F]): StreamT[F, B] =
      $.flatMap(a => StreamT.fromStream(f(a).run.map(_.toStream)))
  }
  def iterateM[A, F[_] : Applicative](a: A)(f: A => OptionT[F, A]): StreamT[F, A] = {
    def notFirst(a: A) = (a, a -> false)
    StreamT.unfoldM((a, true)) {
      case (a, isFirst) => (if (isFirst) OptionT.some(notFirst(a)) else f(a).map(notFirst)).run
    }
  }
  def singleton[F[_] : Applicative, A](value: F[A]): StreamT[F, A] =
    StreamT.fromStream(value.map(Stream(_)))
  def fromEvaluatedIterable[F[_] : Point, A](i: Iterable[A]): StreamT[F, A] = Hoist[StreamT].hoist(
    new (Id ~> F) {
      override def apply[X](fa: scalaz.Id.Id[X]) = Point[F].point(fa)
    }).apply(StreamT.fromIterable(i))
}
