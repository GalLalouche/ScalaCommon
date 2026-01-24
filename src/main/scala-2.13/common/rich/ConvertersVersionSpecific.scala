package common.rich

import java.util

private object ConvertersVersionSpecific {
  def toJava[A](i: Iterator[A]): util.Iterator[A] =
    scala.jdk.CollectionConverters.IteratorHasAsJava(i).asJava
  def knownSize(a: Any): Int = a match {
    case ks: IterableOnce[_] => ks.knownSize
    case _ => -1
  }
}
