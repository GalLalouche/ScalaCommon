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
      $.splitAt(index).thrush(xs => (xs._1.to[ArrayBuffer] += elementToInsert) ++ xs._2)
  }

  def builder[A, B]($ : Seq[A]): mutable.Builder[B, Seq[B]] = $.genericBuilder
  def finishOrderedGroupBy[A, B](
      $ : mutable.LinkedHashMap[A, mutable.Buffer[B]],
  ): Seq[(A, Seq[B])] = $.mapValues(_.toVector).toVector

  def asMap[T, S]($ : Seq[(T, S)]): Map[T, S] = new Map[T, S] {
    override def +[B1 >: S](kv: (T, B1)): Map[T, B1] = throw new UnsupportedOperationException
    override def get(key: T): Option[S] = $.find(_._1 == key).map(_._2)
    override def iterator: Iterator[(T, S)] = $.iterator
    override def toSeq = $
    override def -(key: T): Map[T, S] = throw new UnsupportedOperationException
  }
}
