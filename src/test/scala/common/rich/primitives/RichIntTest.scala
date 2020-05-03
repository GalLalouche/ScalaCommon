package common.rich.primitives

import org.scalatest.FreeSpec

import common.rich.primitives.RichInt._
import common.test.AuxSpecs

class RichIntTest extends FreeSpec with AuxSpecs {
  "factorial works with small numbers" in {
    0.factorial shouldReturn 1
    1.factorial shouldReturn 1
    2.factorial shouldReturn 2
    3.factorial shouldReturn 6
    4.factorial shouldReturn 24
    5.factorial shouldReturn 120
  }

  "choose" - {
    "works with small numbers" in {
      5 choose 2 shouldReturn 10
      6 choose 3 shouldReturn 20
      10 choose 5 shouldReturn 252
    }
  }

  "power" - {
    "throws an exception for 0 ^ 0" in {
      an[IllegalArgumentException] should be thrownBy {
        0 exp 0
      }
    }
    "raises to the power" in {
      2 exp 4 shouldReturn 16
    }
    "works for negative numbers in base" in {
      -1 exp 0 shouldReturn 1
      -1 exp 1 shouldReturn -1
      -2 exp 2 shouldReturn 4
      -2 exp 3 shouldReturn -8
    }
  }

  "permutation works with small numbers" in {
    4 perm 2 shouldReturn 12
    10 perm 3 shouldReturn 720
    5 perm 5 shouldReturn 120
  }

  "padLeftZeros" - {
    "throws if actual size is larger than minSize" in {
      an[IllegalArgumentException] shouldBe thrownBy {123.padLeftZeros(2)}
    }
    "does nothing if length is already minSize" in {
      123.padLeftZeros(3) shouldReturn "123"
    }
    "pads left" in {
      123.padLeftZeros(5) shouldReturn "00123"
    }
  }

  "primes returns a stream of primes" in {
    RichInt.primes.take(10).toList shouldReturn List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  }
}
