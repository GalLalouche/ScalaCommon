package common.rich.collections

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import common.rich.RichT.richT

private object RichSeqSpecVer {
  def at[T]($ : Seq[T], elementToInsert: T, index: Int): Seq[T] = {
    require(index >= 0)
    if ($.size < index)
      throw new IndexOutOfBoundsException(s"requested to remove at $index when size is ${$.size}")
    else
      $.splitAt(index)
        .thrush(xs => (xs._1.iterator.to(ArrayBuffer) += elementToInsert) ++ xs._2)
        .toVector
  }

  def builder[A, B]($ : Seq[A]): mutable.Builder[B, Seq[B]] = $.iterableFactory.newBuilder
  def finishOrderedGroupBy[A, B](
      $ : mutable.LinkedHashMap[A, mutable.Buffer[B]],
  ): Seq[(A, Seq[B])] = $.view.mapValues(_.toVector).toVector

  def asMap[T, S]($ : Seq[(T, S)]): Map[T, S] = new Map[T, S] {
    override def get(key: T): Option[S] = $.find(_._1 == key).map(_._2)
    override def iterator: Iterator[(T, S)] = $.iterator
    override def toSeq = $
    override def removed(key: T): Map[T, S] = throw new UnsupportedOperationException
    override def updated[V1 >: S](key: T, value: V1): Map[T, V1] =
      throw new UnsupportedOperationException
  }
}
