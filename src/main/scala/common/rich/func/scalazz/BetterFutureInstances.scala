package common.rich.func

import scala.concurrent.{ExecutionContext, Future}

import scalaz.MonadError

/**
 * Scalaz's [[scalaz.std.scalaFuture.futureInstance]] has an async point implementation, which is
 * silly.
 */
trait BetterFutureInstances {
  implicit object FuturePoint extends Point[Future] {
    override def point[A](a: => A): Future[A] = Future.successful(a)
  }
  implicit def betterFutureInstances(implicit ec: ExecutionContext): MonadError[Future, Throwable] =
    new MonadError[Future, Throwable] {
      override def raiseError[A](e: Throwable) = Future.failed(e)
      override def handleError[A](fa: Future[A])(f: Throwable => Future[A]) = fa.recoverWith {
        case e => f(e)
      }
      override def bind[A, B](fa: Future[A])(f: A => Future[B]) = fa.flatMap(f)
      override def point[A](a: => A) = Future.successful(a)
    }
}
object BetterFutureInstances extends BetterFutureInstances
