package common.test

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scalaz.Scalaz.ToBindOps

import common.rich.primitives.RichBoolean.richBoolean
import common.test.GenInstances.MonadGen

class MoreGenTest extends PropSpec with ScalaCheckDrivenPropertyChecks with AuxSpecs {
  property("capitalization is case-insensitive equals") {
    forAll(Gen.alphaNumStr.mproduct(MoreGen.capitalization)) { case (s, cap) =>
      s.equals(cap).isFalse ==> s.equalsIgnoreCase(cap)
    }
  }
  property("prefix is a prefix of the required length") {
    forAll(for {
      s <- Gen.alphaNumStr
      if s.nonEmpty
      i <- Gen.choose(0, s.length - 1)
      prefix <- MoreGen.prefix(s, i)
    } yield (s, i, prefix)) { case (s, i, prefix) => prefix.length == i && s.startsWith(prefix) }
  }
  property("oneOf returns a value in in the index") {
    forAll(
      Gen.nonEmptyListOf(Arbitrary.arbInt.arbitrary).map(_.toIndexedSeq).mproduct(MoreGen.oneOf),
    ) { case (s, e) => s.contains(e) }
  }
  property("nonEmptyString is non empty and made up of generated chars") {
    forAll(
      MoreGen.nonEmptyString(Gen.choose('a', 'd')),
    )(s => s.nonEmpty && s.forall(c => c >= 'a' && c <= 'd'))
  }
  property("nonEmptyString default is alpha numeric") {
    forAll(MoreGen.nonEmptyAlphaNumString)(s => s.nonEmpty && s.forall(_.isLetterOrDigit))
  }
}
