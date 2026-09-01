package common.test

import org.scalacheck.{Arbitrary, Gen}

import common.Percentage

object Arbitraries {
  val genPercentage: Gen[Percentage] = Gen.choose(0.0, 1.0).map(Percentage.apply)
  implicit val arbitraryPercentage: Arbitrary[Percentage] = Arbitrary(genPercentage)
}
