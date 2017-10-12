package common.rich.primitives


import common.AuxSpecs
import common.rich.primitives.RichInt._
import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

import scala.concurrent.duration._

class RichIntPropTest extends PropSpec with GeneratorDrivenPropertyChecks with AuxSpecs {
	property("gcd should divide both numbers") {
		forAll(maxDiscarded(100000)) { (x: Int, y: Int) =>
			whenever(x > 0 && y > 0) {
				val gcd = RichInt.gcd(x, y)
				x % gcd shouldReturn 0
				y % gcd shouldReturn 0
			}
		}
	}

	property("gcd should be the largest common divisor") {
		val g = Gen.choose(1, 100000)
		forAll(g, g) { (x: Int, y: Int) =>
			val gcd = RichInt.gcd(x, y)
			for (i <- 1 to Math.max(x, y))
				if (x % i == y % i && x % i == 0)
					i should be <= gcd
		}
	}

	property("is prime is false for perfect squares") {
		forAll(Gen.choose(2, 10000)) { n: Int =>
			(n * n).isPrime shouldReturn false
		}
	}

	property("is prime should return true if the number is prime") {
		forAll(Gen.choose(3, 100000)) { n: Int =>
			if (n.isPrime)
				(2 until n) exists (n % _ == 0) shouldReturn false
		}
	}

	property("is prime should return false if the number is not a prime") {
		forAll(Gen.choose(1, 100000)) { n: Int =>
			if (n.isPrime == false)
				(2 until n) exists (n % _ == 0) shouldReturn true
		}
	}

	property("Primes should return a stream of primes") {
		forAll(Gen.choose(1, 10000)) { n: Int =>
			RichInt.primes.take(n).toList.forall(_.isPrime) shouldReturn true
		}
	}

	property("primes should return an ever increasing stream") {
		forAll(Gen.choose(1, 10000)) { n: Int =>
			val primes = RichInt.primes.take(n).toVector
			for (i <- 0 until primes.size - 1)
				primes(i) - primes(i + 1) should be < 0
		}
	}

	property("primes factorization should return primes only") {
		forAll(Gen.choose(1, 10000)) { n: Int =>
			n.primesFactorization.keys.forall(_.isPrime) shouldReturn true
		}
	}

	property("primes factorization should return only positive amounts") {
		forAll(Gen.choose(1, 10000)) { n: Int =>
			n.primesFactorization.values.forall(_ > 0) shouldReturn true
		}
	}

	property("primes factorization product should be equal to n") {
		forAll(Gen.choose(1, 10000)) { n: Int =>
			n.primesFactorization.map(e => Math.pow(e._1, e._2)).product shouldReturn n
		}
	}
	property("factorization should be QUICK (well, relatively speaking)") {
		forAll(Gen.choose(1e5.toInt, 1e6.toInt), minSuccessful(1000)) { n: Int => {
			n.primesFactorization
		} shouldFinish in lessThan 1.second
		}
	}

	property("euler's totient function should return the number of coprimes integers") {
		forAll(Gen.choose(1, 100000)) { n: Int =>
			val actual = n.eulersTotient
			val expected = (1 to n).map(RichInt.gcd(_, n)).count(_ == 1)
			actual shouldReturn expected
		}
	}

	property("any number choose 0 is 1") {
		forAll { n: Int => whenever(n >= 0) {
			n choose 0 shouldReturn 1
		}
		}
	}

	property("any number choose 1 should is itself") {
		forAll { n: Int => whenever(n >= 0) {
			n choose 1 shouldReturn n
		}
		}
	}

	property("any number choose itself is 1") {
		forAll { n: Int => whenever(n >= 0) {
			n choose n shouldReturn 1
		}
		}
	}

	property("any number perm itself is itself factorized") {
		forAll(Gen.choose(1, 1000)) { n: Int =>
			n perm n shouldReturn n.factorial
		}
	}

	property("any number perm 0 is 1") {
		forAll {
			n: Int => whenever(n >= 0) {
				n perm 0 shouldReturn 1
			}
		}
	}

	property("any number perm 1 is itself") {
		forAll {
			n: Int => whenever(n >= 0) {
				n perm 1 shouldReturn n
			}
		}
	}
}
