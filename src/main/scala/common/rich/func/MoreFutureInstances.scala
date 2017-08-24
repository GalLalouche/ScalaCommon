package common.rich.func
import scala.concurrent.{ExecutionContext, Future}
import scalaz.MonadPlus
import scalaz.std.FutureInstances
import common.rich.RichFuture._

trait MoreFutureInstances extends FutureInstances {
  implicit def FutureMonadPlus(implicit ec: ExecutionContext): MonadPlus[Future] = new MonadPlus[Future] {
    override def bind[A, B](fa: Future[A])(f: A => Future[B]) = fa flatMap f
    override def point[A](a: => A) = Future successful a
    override def empty[A] = Future failed new NoSuchElementException("empty")
    override def plus[A](a: Future[A], b: => Future[A]) = a orElseTry b
  }
}
