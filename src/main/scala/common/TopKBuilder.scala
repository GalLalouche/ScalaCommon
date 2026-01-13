package common

import java.util.PriorityQueue

import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

import common.rich.RichT.richT
import common.rich.primitives.RichBoolean.richBoolean

/** Takes the largest K elements added to it. Returned sequence is sorted in ascending order. */
class TopKBuilder[A](k: Int)(implicit ord: Ordering[A])
    extends mutable.ReusableBuilder[A, Seq[A]]
    with BuilderShimVersionSpecific[A] {
  private val q = new PriorityQueue[A](k, ord.compare)
  override def clear(): Unit = q.clear()
  override def result(): Seq[A] = {
    val mb = Vector.newBuilder[A].<|(_.sizeHint(q.size()))
    while (q.isEmpty.isFalse)
      mb += q.poll()
    // I've measured the performance of using an array and filling it backwards, and while it is
    // faster than doing a reverse on a vector at the end, it's not that much faster, and also the
    // return value isn't persistent.
    mb.result().reverse
  }
  override def addOne(next: A): this.type = {
    if (q.size < k)
      q.add(next)
    else if (q.peek < next) {
      q.poll()
      q.add(next)
    }
    this
  }
}

object TopKBuilder {
  def apply[A](k: Int)(implicit ord: Ordering[A]): TopKBuilder[A] = new TopKBuilder[A](k)
}
