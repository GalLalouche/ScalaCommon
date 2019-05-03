package common.rich.collections

import common.rich.collections.RichIterator._

import scalaz.MonadPlus

/** Like Iterable, but its higher order methods don't invoke a builder. */
class LazyIterable[+A](_iterator: => Iterator[A]) {
  // TODO a type class for Iterable?
  def iterator: Iterator[A] = _iterator

  def mapIterator[B](f: Iterator[A] => Iterator[B]): LazyIterable[B] = new LazyIterable(f(iterator))

  def takeUntil(p: A => Boolean): LazyIterable[A] = mapIterator(_ takeUntil p)
  def takeWhile(p: A => Boolean): LazyIterable[A] = mapIterator(_ takeWhile p)
  def dropWhile(p: A => Boolean): LazyIterable[A] = mapIterator(_ dropWhile p)
  def take(n: Int): LazyIterable[A] = mapIterator(_ take n)
  def drop(n: Int): LazyIterable[A] = mapIterator(_ drop n)

  def foreach[U](f: A => U): Unit = iterator foreach f
  def map[B](f: A => B): LazyIterable[B] = mapIterator(_ map f)
  def flatMap[B](f: A => Iterator[B]): LazyIterable[B] = mapIterator(_.flatMap(f(_)))
  def flatMap[B](f: A => LazyIterable[B])(implicit d: DummyImplicit): LazyIterable[B] =
    flatMap(f.andThen(_.iterator))
  def filter(p: A => Boolean)(implicit d: DummyImplicit): LazyIterable[A] = mapIterator(_ filter p)
  def withFilter(p: A => Boolean)(implicit d: DummyImplicit): LazyIterable[A] = mapIterator(_ filter p)
  def filterNot(p: A => Boolean)(implicit d: DummyImplicit): LazyIterable[A] = mapIterator(_ filterNot p)

  def toVector: Vector[A] = iterator.toVector
  def toStream: Seq[A] = iterator.toStream
}

object LazyIterable {
  def from[A](i: => Iterator[A]): LazyIterable[A] = new LazyIterable[A](i)
  def iterate[A](a: => A)(f: A => A): LazyIterable[A] = new LazyIterable[A](Iterator.iterate(a)(f))
  def continually[A](a: => A): LazyIterable[A] = new LazyIterable[A](Iterator.continually(a))

  implicit object MonadPlusEv extends MonadPlus[LazyIterable] {
    override def empty[A] = from(Iterator.empty)
    override def plus[A](a: LazyIterable[A], b: => LazyIterable[A]) = a.mapIterator(_ ++ b.iterator)
    override def bind[A, B](fa: LazyIterable[A])(f: A => LazyIterable[B]) = fa flatMap f
    override def point[A](a: => A) = from(Iterator(a))

    override def map[A, B](fa: LazyIterable[A])(f: A => B): LazyIterable[B] = fa map f
    override def filter[A](fa: LazyIterable[A])(f: A => Boolean): LazyIterable[A] = fa filter f
  }
}
