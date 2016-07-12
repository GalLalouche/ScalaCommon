package common.rich.primitives

import scala.annotation.tailrec

object RichInt {
  implicit class Rich(n: Int) {
    def factorial: BigInt = {
      @tailrec
      def aux(x: Int, result: BigInt = 1): BigInt = x.ensuring(x >= 0) match {
        case 0 => result
        case _ => aux(x - 1, result * x)
      }
      aux(n)
    }

    def choose(k: Int): BigInt = {
      def aux(n: Int, k: Int): BigInt = (n, k) match {
        case (x, y) if x < 0 || y < 0 || x < y => 0
        case (0, _) => 1
        case (_, 0) => 1
        case (x, 1) => x
        case (x, y) if (x == y) => 1
        case (x, y) => factorial / (new Rich(y).factorial * new Rich(x - y).factorial)
      }
      if (k > n / 2)
        choose(n - k) // slightly faster
      else
        aux(n, k)
    }
    // i.e. nPk
    def perm(k: Int): BigInt = choose(k) * new Rich(k).factorial

    def isPrime: Boolean = 2.to(Math.sqrt(n).toInt).forall(n % _ != 0)

    /** Returns a prime from each primer factor to its power in the factorization */
    def primesFactorization: Map[Int, Int] = {
      require(n > 0)
      if (n == 1)
        Map(1 -> 1)
      var currentN = n
      var primes = RichInt.primes
      var $ = Map[Int, Int]().withDefaultValue(0)
      while (currentN > 1 && new Rich(currentN).isPrime == false)
        if (currentN % primes.head == 0) {
          $ = $.updated(primes.head, $(primes.head) + 1)
          currentN = currentN / primes.head
        }
        else
          primes = primes.tail
      if (currentN != 1)
        $ = $.updated(currentN, $(currentN) + 1)
      $
    }

    def eulersTotient: Int =
      (n * primesFactorization.keys.map(_.toDouble).map(1 - 1.0 / _).product).round.toInt

    def exp(other: Int): BigInt = {
      require(n != 0 || other != 0)
      var $ = BigInt(1)
      for (i <- 1 to other)
        $ *= n
      $
    }
  }
  @tailrec def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  lazy val primes: Stream[Int] = Stream from 2 filter (_.isPrime)
}
