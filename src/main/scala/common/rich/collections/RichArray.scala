package common.rich.collections

object RichArray {
  implicit class richNestedArray[T]($: Array[Array[T]]) {
    def deepSeq: Seq[Seq[T]] = $.toSeq.map(_.toSeq)
    def apply(e: (Int, Int)): T = $(e._1)(e._2)
  }
}
