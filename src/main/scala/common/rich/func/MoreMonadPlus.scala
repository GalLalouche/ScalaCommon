package common.rich.func

import scalaz.MonadPlus
import rx.lang.scala.Observable

object MoreMonadPlus {
  implicit object SeqMonadPlus extends MonadPlus[Seq] {
    override def bind[A, B](fa: Seq[A])(f: (A) => Seq[B]): Seq[B] = fa flatMap f
    override def plus[A](a: Seq[A], b: => Seq[A]): Seq[A] = a ++ b
    override def point[A](a: => A): Seq[A] = Seq(a)
    override def empty[A]: Seq[A] = Seq()
  }
  implicit object TraversableMonadPlus extends MonadPlus[Traversable] {
    override def bind[A, B](fa: Traversable[A])(f: A => Traversable[B]): Traversable[B] = fa flatMap f
    override def plus[A](a: Traversable[A], b: => Traversable[A]): Traversable[A] = a ++ b
    override def point[A](a: => A): Traversable[A] = Traversable(a)
    override def empty[A]: Traversable[A] = Traversable()
  }
  implicit object IterableMonadPlus extends MonadPlus[Iterable] {
    override def bind[A, B](fa: Iterable[A])(f: A => Iterable[B]): Iterable[B] = fa flatMap f
    override def plus[A](a: Iterable[A], b: => Iterable[A]): Iterable[A] = a ++ b
    override def point[A](a: => A): Iterable[A] = Iterable(a)
    override def empty[A]: Iterable[A] = Iterable()
  }
  implicit object SetMonadPlus extends MonadPlus[Set] {
    override def bind[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa flatMap f
    override def plus[A](a: Set[A], b: => Set[A]): Set[A] = a ++ b
    override def point[A](a: => A): Set[A] = Set(a)
    override def empty[A]: Set[A] = Set()
  }
  implicit object ObservableMonadPlus extends MonadPlus[Observable] {
    override def bind[A, B](fa: Observable[A])(f: A => Observable[B]): Observable[B] = fa flatMap f
    override def plus[A](a: Observable[A], b: => Observable[A]): Observable[A] = a ++ b
    override def point[A](a: => A): Observable[A] = Observable.just(a)
    override def empty[A]: Observable[A] = Observable.empty
  }
}
