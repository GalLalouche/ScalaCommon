package common.rich.func.kats

import cats.UnorderedFoldable
import cats.implicits.toUnorderedFoldableOps

import scala.Ordering.Implicits._
import scala.collection.mutable.ListBuffer

import common.rich.primitives.RichBoolean._

trait ToMoreUnorderedFoldableOps {
  implicit class toMoreUnorderedFoldableOps[A, F[_]: UnorderedFoldable]($ : F[A]) {
    def printPerLine(): Unit = $.unorderedFoldMap(println)

    def topK(k: Int)(implicit ord: Ordering[A]): Seq[A] = {
      val q = new java.util.PriorityQueue[A](k, ord.compare)
      $.unorderedFoldMap[Unit] { next =>
        if (q.size < k)
          q.add(next)
        else if (q.peek < next) {
          q.poll()
          q.add(next)
        }
      }
      val mb = new ListBuffer[A]()
      while (q.isEmpty.isFalse)
        q.poll() +=: mb
      mb.toVector
    }

    def bottomK(k: Int)(implicit ord: Ordering[A]): Seq[A] = topK(k)(ord.reverse)
  }

}

object ToMoreUnorderedFoldableOps extends ToMoreUnorderedFoldableOps
