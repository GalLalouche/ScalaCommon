package common.rich.func.kats

import cats.{Order, UnorderedFoldable}
import cats.implicits.{catsKernelStdCommutativeMonoidForOption, catsSyntaxPartialOrder, toUnorderedFoldableOps}
import cats.kernel.CommutativeMonoid

import scala.Ordering.Implicits._
import scala.collection.mutable.ListBuffer

import common.rich.func.kats.Monoids.{Max, Min}

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

    // For some reason, these are only defined on ordered foldables.
    def minimumOption(implicit A: Order[A]): Option[A] =
      $.unorderedFoldMap(e => Option(Min(e))).map(_.getMin)
    def maximumOption(implicit A: Order[A]): Option[A] =
      $.unorderedFoldMap(e => Option(Max(e))).map(_.getMax)

    def minimumByOption[B: Order](f: A => B): Option[A] = $.minimumOption(Order.by(f))
    def maximumByOption[B: Order](f: A => B): Option[A] = $.maximumOption(Order.by(f))

    def minimumList(implicit A: Order[A]): List[A] =
      $.unorderedFoldMap(List(_))(ToMoreUnorderedFoldableOps.MinListCO)
    def maximumList(implicit A: Order[A]): List[A] =
      $.unorderedFoldMap(List(_))(ToMoreUnorderedFoldableOps.MinListCO(Order.reverse(Order[A])))
  }
}

object ToMoreUnorderedFoldableOps extends ToMoreUnorderedFoldableOps {
  private implicit def MinListCO[A: Order]: CommutativeMonoid[List[A]] = CommutativeMonoid.instance(
    Nil,
    {
      case (xs @ (x :: _), ys @ (y :: _)) => if (x < y) xs else if (x > y) ys else xs ++ ys
      case (xs, Nil) => xs
      case (Nil, ys) => ys
    },
  )
}
