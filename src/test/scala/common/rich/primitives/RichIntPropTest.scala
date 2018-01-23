package common.rich.primitives

import common.AuxSpecs
import common.rich.primitives.RichBoolean._
import common.rich.primitives.RichInt._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class RichIntPropTest extends PropSpec with GeneratorDrivenPropertyChecks with AuxSpecs {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 5)
  private implicit val arbSmallPositive: Arbitrary[Int] = Arbitrary(Gen.choose(1, 1000))
  private def nProp(propName: String, prop: Int => Unit): Unit =
    property(propName) {
      forAll { n: Int => prop(n) }
    }
  private def nkProp(propName: String, prop: (Int, Int) => Unit): Unit =
    property(propName) {
      forAll { (n: Int, k: Int) => prop(n, k) }
    }

  nProp("n! is divided by all numbers from 1 to n", n => {
    val fact = n.factorial
    1 to n allShouldSatisfy (fact % _ == 0)
  })

  nProp("n ^ 0 is equal to 1", _ exp 0 shouldReturn 1)
  nProp("0 ^ n is equal to 0", 0 exp _ shouldReturn 0)

  nkProp("gcd should divide both numbers", (n, k) => {
    val gcd = RichInt.gcd(n, k)
    n % gcd shouldReturn 0
    k % gcd shouldReturn 0
  })
  nkProp("gcd should be the largest common divisor", (n, k) => {
    val gcd = RichInt.gcd(n, k)
    for (i <- 1 to Math.max(n, k))
      if (n % i == k % i && n % i == 0)
        i should be <= gcd
  })

  nkProp("is prime is false for composite numbers", (n, k) => whenever(n > 1 && k > 1) {
    (n * k).isPrime shouldReturn false
  })
  nProp("is prime should return true if the number has no dividers",
    n => (2 until n) exists (n % _ == 0) shouldReturn n.isPrime.isFalse)

  nProp("prime factorization should return primes only",
    _.primesFactorization.keys allShouldSatisfy (_.isPrime))
  nProp("prime factorization should return only positive am",
    _.primesFactorization.values allShouldSatisfy (_ > 0))
  nProp("prime factorization product should be equal to n",
    n => n.primesFactorization.map(e => Math.pow(e._1, e._2)).product shouldReturn n)

  nProp("euler's totient function should return the number of coprime integers", n => {
    val actual = n.eulersTotient
    val expected = (1 to n).map(RichInt.gcd(_, n)).count(_ == 1)
    actual shouldReturn expected
  })

  nProp("n choose 0 is 1", _ choose 0 shouldReturn 1)
  nProp("n choose 1 should is i", n => n choose 1 shouldReturn n)
  nProp("n choose itself is 1", n => n choose n shouldReturn 1)
  nkProp("n choose k is equal to n choose (n - k)", (n, k) => n choose k shouldReturn (n choose (n - k)))

  nProp("n perm itself is itself factor", n => n perm n shouldReturn n.factorial)
  nProp("n perm 0 is 1", _ perm 0 shouldReturn 1)
  nProp("n perm 1 is itself", n => n perm 1 shouldReturn n)
}
