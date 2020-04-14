package common.rich.func

import scala.concurrent.Future
import scala.language.higherKinds

trait Point[F[_]] {
  def point[A](a: => A): F[A]
}
object Point extends PointLowPriorityImplicits {
  // The default point for Future in Scalaz is async for some odd reason.
  implicit object FuturePoint extends Point[Future] {
    override def point[A](a: => A): Future[A] = Future.successful(a)
  }
  @inline def apply[F[_]](implicit F: Point[F]): Point[F] = F
}
