package common.rich

import java.util

import scala.collection.JavaConverters.asJavaIteratorConverter

private object ConvertersVersionSpecific {
  def toJava[A](i: Iterator[A]): util.Iterator[A] = asJavaIteratorConverter(i).asJava
  // Not supported in Scala 2.12 ¯\_(ツ)_/¯
  def knownSize(a: Any): Int = -1
}
