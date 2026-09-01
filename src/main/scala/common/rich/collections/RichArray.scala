package common.rich.collections

import scala.collection.mutable
import scala.reflect.ClassTag

import common.{BuilderShimVersionSpecific, UtilsVersionSpecific}

object RichArray {
  implicit class richNestedArray[T](private val $ : Array[Array[T]]) extends AnyVal {
    def deepSeq: Seq[Seq[T]] = $.toSeq.map(_.toSeq)
    def apply(e: (Int, Int)): T = $(e._1)(e._2)
  }

  def arraySeqBuilder[T: ClassTag]: mutable.ReusableBuilder[T, Seq[T]] =
    new mutable.ReusableBuilder[T, Seq[T]] with BuilderShimVersionSpecific[T] {
      private val $ = mutable.ArrayBuilder.make[T]
      override def clear(): Unit = $.clear()
      override def result() = UtilsVersionSpecific.unsafeArray($.result())
      override def addOne(elem: T) = { $.+=(elem); this }
    }
}
