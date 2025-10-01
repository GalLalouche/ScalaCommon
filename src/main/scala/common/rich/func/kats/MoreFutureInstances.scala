package common.rich.func.kats

import alleycats.Pure
import cats.CommutativeMonad

import scala.concurrent.{ExecutionContext, Future}

trait MoreFutureInstances {
  /**
   * Since the regular [[cats.Applicative]] instance requires an `implicit`
   * [[scala.concurrent.ExecutionContext]].
   */
  implicit object futureIsPure extends Pure[Future] {
    override def pure[A](a: A): Future[A] = Future.successful(a)
  }

  /** Is this lawful? Meh. */
  implicit def futureIsCommutativeMonad(implicit
      ec: ExecutionContext,
  ): CommutativeMonad[Future] = new CommutativeMonad[Future] {
    override def pure[A](x: A): Future[A] = Future.successful(x)
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => Future[Either[A, B]]): Future[B] =
      cats.instances.future.catsStdInstancesForFuture.tailRecM(a)(f)
  }
}
object MoreFutureInstances extends MoreFutureInstances
