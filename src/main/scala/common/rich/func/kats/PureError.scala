package common.rich.func.kats

import scala.concurrent.Future
import scala.util.{Failure, Try}

trait PureError[F[_], S] {
  def pureError[A](e: S): F[A]
}

object PureError {
  implicit object OptionIsPureError extends PureError[Option, Unit] {
    override def pureError[A](e: Unit): Option[A] = None
  }
  implicit object TryIsPureError extends PureError[Try, Throwable] {
    override def pureError[A](e: Throwable): Try[A] = Failure(e)
  }
  implicit object FutureIsPureError extends PureError[Future, Throwable] {
    override def pureError[A](e: Throwable): Future[A] = Future.failed(e)
  }
  def tryWrap[F[_], A](f: => F[A])(implicit F: PureError[F, Throwable]): F[A] =
    try
      f
    catch {
      case e: Throwable => F.pureError(e)
    }
}
