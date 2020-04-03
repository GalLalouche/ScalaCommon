package common.rich.func

import scala.collection.immutable.{IndexedSeq => ImmutableIndexedSeq}

import scalaz.{Foldable, MonadPlus, Monoid}

object MoreEnumeratumInstances {
  implicit object ImmutableImmutableIndexedSeqEv
      extends MonadPlus[ImmutableIndexedSeq] with Foldable[ImmutableIndexedSeq] {
    override def bind[A, B](fa: ImmutableIndexedSeq[A])(f: A => ImmutableIndexedSeq[B]) = fa flatMap f
    override def point[A](a: => A) = ImmutableIndexedSeq(a)
    override def empty[A] = ImmutableIndexedSeq.empty
    override def plus[A](a: ImmutableIndexedSeq[A], b: => ImmutableIndexedSeq[A]) = a ++ b

    // Optimized implementation
    override def foldRight[A, B](fa: ImmutableIndexedSeq[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
    override def foldMap[A, B: Monoid](fa: ImmutableIndexedSeq[A])(f: A => B): B =
      fa.map(f).fold(Monoid[B].zero)(Monoid[B].append(_, _))
    override def foldLeft[A, B](fa: ImmutableIndexedSeq[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
  }
}
