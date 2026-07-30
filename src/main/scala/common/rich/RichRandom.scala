package common.rich

import scala.Numeric.Implicits._
import scala.math.Ordered.orderingToOrdered
import scala.util.Random

import common.Percentage

object RichRandom {
  implicit class richRandom(private val $ : Random) extends AnyVal {
    def flipCoin(p: Percentage): Boolean = $.nextDouble() <= p.p
    def selectW[A](xs: Iterable[(Double, A)]): A =
      selectWAux[Double, A](xs, $.nextDouble().*)
    def selectW[A](xs: Iterable[(Int, A)])(implicit dummyImplicit: DummyImplicit): A =
      selectWAux[Int, A](xs, $.nextInt(_))

    private def selectWAux[N: Numeric, A](xs: Iterable[(N, A)], rGen: N => N): A = {
      val zero = implicitly[Numeric[N]].fromInt(0)
      val total = xs.view.map(_._1).sum.ensuring(_ > zero)
      val r = rGen(total)
      val iterator = xs.iterator
      var sum = zero
      while (true) {
        val (count, a) = iterator.next()
        sum += count
        if (sum > r)
          return a
      }
      throw new AssertionError("while (true) above")
    }
  }
}
