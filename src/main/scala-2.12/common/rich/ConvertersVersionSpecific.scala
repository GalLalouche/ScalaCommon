package common.rich

import java.util

import scala.collection.JavaConverters.{asJavaIteratorConverter, mapAsScalaMap}

private[common] object ConvertersVersionSpecific {
  def toJava[A](i: Iterator[A]): util.Iterator[A] = asJavaIteratorConverter(i).asJava
  def toScala[K, V](m: util.Map[K, V]): scala.collection.mutable.Map[K, V] =
    mapAsScalaMap(m)
  def distinctBy[A, B](i: Iterable[A], f: A => B): Iterable[A] = new Iterable[A] {
    override def iterator: Iterator[A] = {
      val seen = scala.collection.mutable.HashSet[B]()
      i.iterator.filter { a =>
        val key = f(a)
        if (seen.contains(key))
          false
        else {
          seen.add(key)
          true
        }
      }
    }
  }
  // Not supported in Scala 2.12 ¯\_(ツ)_/¯
  def knownSize(a: Any): Int = -1
}
