package common.rich.func.kats

import cats.{Monad, UnorderedFoldable}
import cats.implicits.catsSyntaxSemigroup
import cats.kernel.CommutativeMonoid

import scala.collection.View

trait ViewInstances {
  implicit object viewInstances extends Monad[View] with UnorderedFoldable[View] {
    override def unorderedFoldMap[A, B: CommutativeMonoid](fa: View[A])(f: A => B): B =
      fa.foldLeft(CommutativeMonoid[B].empty)((b, a) => b |+| f(a))
    override def pure[A](x: A): View[A] = View(x)
    override def flatMap[A, B](fa: View[A])(f: A => View[B]): View[B] = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => View[Either[A, B]]): View[B] =
      MoreIterableInstances.iterableInstances.tailRecM(a)(f(_)).view
  }
}
object ViewInstances extends ViewInstances
