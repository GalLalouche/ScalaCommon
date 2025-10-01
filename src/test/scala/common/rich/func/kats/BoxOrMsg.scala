package common.rich.func.kats

import cats.MonadError

import scala.annotation.tailrec

private sealed trait BoxOrMsg[+T] {
  def get: T
  def getFailure: String
}
private case class Box[T](get: T) extends BoxOrMsg[T] {
  override def getFailure: String = throw new Exception(s"Box <$this> isn't a failure")
}
private case class Msg(getFailure: String) extends BoxOrMsg[Nothing] {
  override def get: Nothing = throw new Exception(s"Error <$this> isn't a box")
}
private object BoxOrMsg {
  implicit object MonadErrorImpl extends MonadError[BoxOrMsg, String] {
    override def raiseError[A](e: String): BoxOrMsg[A] = Msg(e)
    override def handleErrorWith[A](fa: BoxOrMsg[A])(f: String => BoxOrMsg[A]): BoxOrMsg[A] =
      fa match {
        case b @ Box(_) => b
        case Msg(e) => f(e)
      }
    override def flatMap[A, B](fa: BoxOrMsg[A])(f: A => BoxOrMsg[B]): BoxOrMsg[B] =
      fa match {
        case e @ Msg(_) => e
        case Box(e) => f(e)
      }
    override def pure[A](x: A): BoxOrMsg[A] = Box(x)
    override def tailRecM[A, B](a: A)(f: A => BoxOrMsg[Either[A, B]]): BoxOrMsg[B] = {
      @tailrec def go(b: BoxOrMsg[Either[A, B]]): BoxOrMsg[B] = b match {
        case Box(Right(b)) => Box(b)
        case m: Msg => m
        case Box(Left(a)) => go(f(a))
      }
      go(f(a))
    }
  }
  def apply[A](a: A): BoxOrMsg[A] = Box(a)
}
