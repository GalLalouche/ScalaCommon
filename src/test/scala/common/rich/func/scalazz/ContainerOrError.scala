package common.rich.func.scalazz

import scalaz.MonadError

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
    override def handleError[A](fa: ContainerOrError[A])(f: Throwable => ContainerOrError[A]) =
      fa match {
        case e@Container(_) => e
        case Error(t) => f(t)
      }
    override def bind[A, B](fa: ContainerOrError[A])(f: A => ContainerOrError[B]) = fa match {
      case e@Error(_) => e
      case Container(e) => f(e)
    }
    override def point[A](a: => A): ContainerOrError[A] = Container(a)
  }

}