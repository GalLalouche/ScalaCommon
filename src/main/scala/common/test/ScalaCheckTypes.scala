package common.test

import org.scalacheck.{Arbitrary, Gen}

object ScalaCheckTypes {
  case class AlphaNumericString(s: String) extends AnyVal
  implicit val ArbitraryAlphaNumericString: Arbitrary[AlphaNumericString] =
    Arbitrary(Gen.alphaNumStr.map(AlphaNumericString))
  /** Useful when a large intersection between the strings is needed. */
  case class BinaryString(s: String) extends AnyVal
  implicit val ArbitrarySimpleString: Arbitrary[BinaryString] = Arbitrary(
    binaryString.map(BinaryString),
  )
  def binaryString: Gen[String] =
    implicitly[Arbitrary[Seq[Int]]].arbitrary.map(_.view.map(_.abs % 2).mkString)
}
