package common.rich.primitives

import common.rich.primitives.RichInt._
import org.scalatest.{FlatSpec, Matchers}

class RichIntTest extends FlatSpec with Matchers {
  "factorial" should "work with small numbers" in {
    0.factorial should be === 1
    1.factorial should be === 1
    2.factorial should be === 2
    3.factorial should be === 6
    4.factorial should be === 24
    5.factorial should be === 120
  }

  "choose" should "work with small numbers" in {
    5 choose 2 should be === 10
    10 choose 1 should be === 10
    100 choose 0 should be === 1
  }

  it should "work with binomial complement" in {
    5 choose 5 should be === 1
    5 choose 4 should be === 5
    5 choose 3 should be === 10
  }

  it should "work with slightly larger numbers" in {
    6 choose 3 should be === 20
  }

  "power" should "return 1 when upping to zero" in {
    4 exp 0 should be === 1
  }

  it should "return 0 when 0 is times something" in {
    0 exp 4 should be === 0
  }

  it should "throw an exception for 0 ^ 0" in {
    an[IllegalArgumentException] should be thrownBy {
      0 exp 0
    }
  }

  it should "raise to the power" in {
    2 exp 4 should be === 16
  }

  it should "work for negative numbers in base" in {
    -1 exp 0 should be === 1
    -1 exp 1 should be === -1
    -1 exp 2 should be === 1
    -1 exp 3 should be === -1
  }

  "permutation choose" should "work with small numbers" in {
    4 perm 2 should be === 12
    10 perm 3 should be === 720
    5 perm 5 should be === 120
  }

  "primes" should "return a stream of primes" in {
    RichInt.primes.take(10).toList should be === List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  }
}
