package common.rich.collections

import java.util

import scala.reflect.ClassTag

object RichJavaStream {
  implicit class richJavaStream[A <: AnyRef](private val $ : util.stream.Stream[A]) extends AnyVal {
    def toTypedArray(implicit ct: ClassTag[A]): Array[A] = $.toArray[A](ct.newArray(_))
  }
}
