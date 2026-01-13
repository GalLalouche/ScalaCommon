package common.rich.func.kats

import cats.{Applicative, Order, UnorderedFoldable}
import cats.implicits.{catsKernelStdCommutativeMonoidForOption, catsSyntaxPartialOrder, toFunctorOps, toUnorderedFoldableOps}
import cats.kernel.CommutativeMonoid

import common.rich.func.kats.Monoids.{GUnitCommutativeMonoid, Max, Min}

import common.TopKBuilder

trait ToMoreUnorderedFoldableOps {
  implicit class toMoreUnorderedFoldableOps[A, F[_]: UnorderedFoldable]($ : F[A]) {
    def printPerLine(): Unit = $.unorderedFoldMap(println)

    def topK(k: Int)(implicit ord: Ordering[A]): Seq[A] = {
      val heap = TopKBuilder[A](k)
      $.unorderedFoldMap[Unit](heap.addOne)
      heap.result()
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

    def unorderedTraverse_[G[_]: Applicative, B](f: A => G[B]): G[Unit] =
      $.unorderedFoldMap(a => f(a).void)
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
