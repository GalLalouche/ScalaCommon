package common.rich.func.kats

import cats.{Functor, UnorderedFoldable}
import cats.implicits.catsSyntaxSemigroup
import cats.kernel.CommutativeMonoid

import scala.collection.IndexedSeqView

trait IndexedSeqViewInstances {
  implicit object indexedSeqViewInstances
      extends Functor[IndexedSeqView]
      with UnorderedFoldable[IndexedSeqView] {
    override def unorderedFoldMap[A, B: CommutativeMonoid](fa: IndexedSeqView[A])(f: A => B): B =
      fa.foldLeft(CommutativeMonoid[B].empty)((b, a) => b |+| f(a))

    // Optimized overloads
    override def map[A, B](fa: IndexedSeqView[A])(f: A => B): IndexedSeqView[B] = fa.map(f)
  }
}
object IndexedSeqViewInstances extends IndexedSeqViewInstances
