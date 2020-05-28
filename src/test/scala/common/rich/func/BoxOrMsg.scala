package common.rich.func

import scalaz.MonadError

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
    override def handleError[A](fa: BoxOrMsg[A])(f: String => BoxOrMsg[A]): BoxOrMsg[A] =
      fa match {
        case b@Box(_) => b
        case Msg(e) => f(e)
      }
    override def bind[A, B](fa: BoxOrMsg[A])(f: A => BoxOrMsg[B]): BoxOrMsg[B] =
      fa match {
        case e@Msg(_) => e
        case Box(e) => f(e)
      }
    override def point[A](a: => A): BoxOrMsg[A] = Box(a)
  }
  def apply[A](a: A): BoxOrMsg[A] = Box(a)
}
