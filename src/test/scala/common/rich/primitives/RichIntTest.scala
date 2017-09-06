package common.rich.primitives

import common.AuxSpecs
import common.rich.primitives.RichInt._
import org.scalatest.FlatSpec

class RichIntTest extends FlatSpec with AuxSpecs {
  "factorial" should "work with small numbers" in {
    0.factorial shouldReturn 1
    1.factorial shouldReturn 1
    2.factorial shouldReturn 2
    3.factorial shouldReturn 6
    4.factorial shouldReturn 24
    5.factorial shouldReturn 120
  }

  "choose" should "work with small numbers" in {
    5 choose 2 shouldReturn 10
    10 choose 1 shouldReturn 10
    100 choose 0 shouldReturn 1
  }

  it should "work with binomial complement" in {
    5 choose 5 shouldReturn 1
    5 choose 4 shouldReturn 5
    5 choose 3 shouldReturn 10
  }

  it should "work with slightly larger numbers" in {
    6 choose 3 shouldReturn 20
  }

  "power" should "return 1 when upping to zero" in {
    4 exp 0 shouldReturn 1
  }

  it should "return 0 when 0 is times something" in {
    0 exp 4 shouldReturn 0
  }

  it should "throw an exception for 0 ^ 0" in {
    an[IllegalArgumentException] should be thrownBy {
      0 exp 0
    }
  }

  it should "raise to the power" in {
    2 exp 4 shouldReturn 16
  }

  it should "work for negative numbers in base" in {
    -1 exp 0 shouldReturn 1
    -1 exp 1 shouldReturn -1
    -1 exp 2 shouldReturn 1
    -1 exp 3 shouldReturn -1
  }

  "permutation choose" should "work with small numbers" in {
    4 perm 2 shouldReturn 12
    10 perm 3 shouldReturn 720
    5 perm 5 shouldReturn 120
  }

  "primes" should "return a stream of primes" in {
    RichInt.primes.take(10).toList shouldReturn List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  }
}
