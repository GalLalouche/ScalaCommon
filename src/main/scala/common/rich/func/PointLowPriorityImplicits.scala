package common.rich.func

import scala.language.higherKinds

import scalaz.Applicative

trait PointLowPriorityImplicits {
  implicit def applicativeToPoint[F[_] : Applicative]: Point[F] = new Point[F] {
    override def point[A](a: => A) = implicitly[Applicative[F]].point(a)
  }
}
