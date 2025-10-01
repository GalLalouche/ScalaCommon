package common.rich.func.scalazz

import scalaz.Applicative

trait Point[F[_]] {
  def point[A](a: => A): F[A]
}
object Point {
  @inline def apply[F[_]](implicit F: Point[F]): Point[F] = F
  implicit def applicativeToPoint[F[_]: Applicative]: Point[F] = new Point[F] {
    override def point[A](a: => A) = implicitly[Applicative[F]].point(a)
  }
}
