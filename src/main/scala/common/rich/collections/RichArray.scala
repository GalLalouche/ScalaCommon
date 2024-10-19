package common.rich.collections

object RichArray {
  implicit class richNestedArray[T](private val $ : Array[Array[T]]) extends AnyVal {
    def deepSeq: Seq[Seq[T]] = $.toSeq.map(_.toSeq)
    def apply(e: (Int, Int)): T = $(e._1)(e._2)
  }
}
