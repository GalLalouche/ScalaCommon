package common.rich

import java.util

import scala.collection.View.DistinctBy

private object ConvertersVersionSpecific {
  def distinctBy[A, B](i: Iterable[A], f: A => B): Iterable[A] = new DistinctBy[A, B](i, f)
  def toJava[A](i: Iterator[A]): util.Iterator[A] =
    scala.jdk.CollectionConverters.IteratorHasAsJava(i).asJava
  def knownSize(a: Any): Int = a match {
    case ks: IterableOnce[_] => ks.knownSize
    case _ => -1
  }
}
