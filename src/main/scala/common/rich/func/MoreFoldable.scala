package common.rich.func

import scalaz.{Foldable, Monoid}

object MoreFoldable {
  implicit object TraversableFoldable extends Foldable[Traversable] {
    override def foldMap[A, B : Monoid](fa: Traversable[A])(f: A => B): B =
      fa.map(f).fold(implicitly[Monoid[B]].zero)(implicitly[Monoid[B]].append(_, _))
    override def foldRight[A, B](fa: Traversable[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
  }
  implicit object IterableFoldable extends Foldable[Iterable] {
    override def foldMap[A, B : Monoid](fa: Iterable[A])(f: A => B): B =
      fa.map(f).fold(implicitly[Monoid[B]].zero)(implicitly[Monoid[B]].append(_, _))
    override def foldRight[A, B](fa: Iterable[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
  }
  implicit object SeqFoldable extends Foldable[Seq] {
    override def foldMap[A, B : Monoid](fa: Seq[A])(f: A => B): B =
      fa.map(f).fold(implicitly[Monoid[B]].zero)(implicitly[Monoid[B]].append(_, _))
    override def foldRight[A, B](fa: Seq[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
  }
}
