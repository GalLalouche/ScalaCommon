package common.rich

import java.util

import scala.collection.View.DistinctBy

private[common] object ConvertersVersionSpecific {
  def distinctBy[A, B](i: Iterable[A], f: A => B): Iterable[A] = new DistinctBy[A, B](i, f)
  def toJava[A](i: Iterator[A]): util.Iterator[A] =
    scala.jdk.CollectionConverters.IteratorHasAsJava(i).asJava
  def toScala[K, V](m: util.Map[K, V]): scala.collection.mutable.Map[K, V] =
    scala.jdk.CollectionConverters.MapHasAsScala(m).asScala
  def knownSize(a: Any): Int = a match {
    case ks: IterableOnce[_] => ks.knownSize
    case _ => -1
  }
}
