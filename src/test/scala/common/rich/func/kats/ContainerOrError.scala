package common.rich.func.kats

import cats.MonadError

import scala.annotation.tailrec

private sealed trait ContainerOrError[+T] {
  def get: T
  def getFailure: Throwable
}
private case class Container[T](get: T) extends ContainerOrError[T] {
  override def getFailure: Throwable = throw new Exception(s"Box <$this> isn't a failure")
}
private case class Error(getFailure: Throwable) extends ContainerOrError[Nothing] {
  override def get: Nothing = throw new Exception(s"Error <$this> isn't a box")
}
private object ContainerOrError {
  implicit object MonadErrorImpl extends MonadError[ContainerOrError, Throwable] {
    override def raiseError[A](e: Throwable): ContainerOrError[A] = Error(e)
    override def handleErrorWith[A](fa: ContainerOrError[A])(f: Throwable => ContainerOrError[A]) =
      fa match {
        case e: Container[_] => e
        case Error(t) => f(t)
      }
    override def flatMap[A, B](fa: ContainerOrError[A])(f: A => ContainerOrError[B]) = fa match {
      case e @ Error(_) => e
      case Container(e) => f(e)
    }
    override def pure[A](a: A): ContainerOrError[A] = Container(a)
    override def tailRecM[A, B](
        a: A,
    )(f: A => ContainerOrError[Either[A, B]]): ContainerOrError[B] = {
      @tailrec def go(c: ContainerOrError[Either[A, B]]): ContainerOrError[B] = c match {
        case Container(Right(b)) => Container(b)
        case e: Error => e
        case Container(Left(a)) => go(f(a))
      }
      go(f(a))
    }
  }
}
