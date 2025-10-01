package common.rich.func.scalazz

import scala.collection.View

import scalaz.{Foldable, Monad, Monoid}

trait ViewInstances {
  implicit object viewInstances extends Monad[View] with Foldable[View] {
    override def foldMap[A, B](fa: View[A])(f: A => B)(implicit F: Monoid[B]): B =
      fa.map(f).fold(F.zero)(F.append(_, _))
    override def foldRight[A, B](fa: View[A], z: => B)(f: (A, => B) => B): B =
      // This isn't actually lazy, but then again, neither is scalaz's Iterable version ¯\_(ツ)_/¯.
      fa.foldRight(z)(f(_, _))
    override def foldLeft[A, B](fa: View[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)
    override def point[A](a: => A): View[A] = View(a)
    override def bind[A, B](fa: View[A])(f: A => View[B]): View[B] = fa.flatMap(f)
  }
}
object ViewInstances extends ViewInstances
