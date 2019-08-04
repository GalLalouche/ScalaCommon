package common.rich.func

import scala.util.{Failure, Success, Try}

import scalaz.{MonadError, MonadPlus}
import scalaz.std.TryInstances

object MoreTryInstances {
  implicit object TryMonadPlus extends TryInstances with MonadPlus[Try] with MonadError[Try, Throwable] {
    override def bind[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
    override def empty[A]: Try[A] = Failure(new NoSuchElementException)
    override def plus[A](a: Try[A], b: => Try[A]): Try[A] = if (a.isSuccess) a else b
    override def point[A](a: => A): Try[A] = Success(a)
    override def raiseError[A](e: Throwable): Try[A] = Failure(e)
    override def handleError[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] =
      if (fa.isSuccess) fa else fa.recoverWith {case e: Throwable => f(e)}
  }
}
