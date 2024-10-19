package common.rich.func

import scala.language.higherKinds

import scalaz.{~>, Applicative, Hoist, Monad, OptionT, StreamT}
import scalaz.Id.Id
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.functor.ToFunctorOps

object RichStreamT {
  implicit class richStreamT[F[_], A](private val $ : StreamT[F, A]) extends AnyVal {
    def last(implicit ev: Monad[F]): F[A] = $.toStream.map(_.last)
    def oMapM[B](f: A => OptionT[F, B])(implicit ev: Applicative[F]): StreamT[F, B] =
      $.flatMap(a => StreamT.fromStream(f(a).run.map(_.toStream)))
    def subFlatMap[B](f: A => Stream[B])(implicit ev: Applicative[F]): StreamT[F, B] =
      $.flatMap(f andThen fromStream[F, B])
    def unconsBatch(n: Int)(implicit ev: Monad[F]): F[(Seq[A], StreamT[F, A])] =
      if (n == 0)
        ev.point(Nil -> $)
      else
        $.uncons.flatMap {
          case None => ev.point(Nil -> $)
          case Some(value) =>
            value._2.unconsBatch(n - 1).map(TuplePLenses.tuple2First.modify(value._1 :: _.toList))
        }
  }

  def iterateM[F[_]: Applicative, A](a: A)(f: A => OptionT[F, A]): StreamT[F, A] = {
    def notFirst(a: A) = (a, a -> false)
    StreamT.unfoldM((a, true)) { case (a, isFirst) =>
      (if (isFirst) OptionT.some(notFirst(a)) else f(a).map(notFirst)).run
    }
  }

  def fillM[F[_]: Applicative, A](a: => OptionT[F, A]): StreamT[F, A] =
    iterateM[F, Either[Unit, A]](Left(Unit)) {
      case Left(_) => a.map(Right(_))
      case Right(_) => a.map(Right(_))
    }.tail.map(_.fold(_ => ???, identity))

  def singleton[F[_]: Applicative, A](value: F[A]): StreamT[F, A] =
    StreamT.fromStream(value.map(Stream(_)))
  def fromStream[F[_]: Applicative, A](s: Stream[A]): StreamT[F, A] =
    StreamT.fromStream(Applicative[F].point(s))
  def fromEvaluatedIterable[F[_]: Point, A](i: Iterable[A]): StreamT[F, A] = Hoist[StreamT]
    .hoist(new (Id ~> F) {
      override def apply[X](fa: scalaz.Id.Id[X]) = Point[F].point(fa)
    })
    .apply(StreamT.fromIterable(i))
}
