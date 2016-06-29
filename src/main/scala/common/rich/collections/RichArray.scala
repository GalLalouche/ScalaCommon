package common.rich.collections

import scala.reflect.ClassTag

object RichArray {
  implicit def richArray[T]($: Array[Array[T]]) = new {
    def deepSeq: Seq[Seq[T]] = $.toSeq.map(_.toSeq)
    def apply(e: (Int, Int)) = $(e._1)(e._2)
  }
  implicit def richSeqArray[T]($: Seq[Seq[T]])(implicit m: ClassTag[T]) = new {
    def deepArray: Array[Array[T]] = $.map(_.toArray).toArray
    def apply(e: (Int, Int)) = $(e._1)(e._2)
  }
}
