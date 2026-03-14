package common.test

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.rng.Seed

import scala.util.Random

object GenOps {
  implicit class RichGen[A](private val $ : Gen[A]) extends AnyVal {
    // noinspection AccessorLikeMethodIsEmptyParen; sample is actually a side-effectful method.
    def getSample(): A = $.sample.get
    def getSample(r: Random): A = $(Gen.Parameters.default, Seed(r.nextLong())).get
  }
  implicit class RichArbitrary[A](private val $ : Arbitrary[A]) extends AnyVal {
    // noinspection AccessorLikeMethodIsEmptyParen; sample is actually a side-effectful method.
    def getSample(): A = $.arbitrary.getSample()
    def getSample(r: Random): A = $.arbitrary(Gen.Parameters.default, Seed(r.nextLong())).get
  }
}
