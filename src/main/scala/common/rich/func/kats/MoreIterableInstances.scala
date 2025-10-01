package common.rich.func.kats

import cats.{Monad, MonoidK}

trait MoreIterableInstances {
  implicit object iterableInstances extends MonoidK[Iterable] with Monad[Iterable] {
    override def empty[A]: Iterable[A] = Iterable.empty
    override def combineK[A](x: Iterable[A], y: Iterable[A]): Iterable[A] = x ++ y
    override def pure[A](x: A): Iterable[A] = Iterable(x)
    override def flatMap[A, B](fa: Iterable[A])(f: A => Iterable[B]): Iterable[B] = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => Iterable[Either[A, B]]): Iterable[B] =
      new Iterable[B] { // Not using Iterator.from for source backward compatibility With 2.12.
        override def iterator = new IteratorSeeker[A, B](a, f(_).iterator)
      }
    // Optimized overrides
    override def map[A, B](fa: Iterable[A])(f: A => B): Iterable[B] = fa.map(f)
  }
}

object MoreIterableInstances extends MoreIterableInstances
