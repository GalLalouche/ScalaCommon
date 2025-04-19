package common.test

import java.time.LocalDate

import org.scalacheck.{Arbitrary, Gen}

object ScalaCheckTypes {
  case class AlphaNumericString(s: String) extends AnyVal
  implicit val ArbitraryAlphaNumericString: Arbitrary[AlphaNumericString] =
    Arbitrary(Gen.alphaNumStr.map(AlphaNumericString))
  /** Useful when a large intersection between the strings is needed. */
  case class BinaryString(s: String) extends AnyVal
  implicit val ArbitrarySimpleString: Arbitrary[BinaryString] =
    Arbitrary(binaryString.map(BinaryString))
  def binaryString: Gen[String] =
    implicitly[Arbitrary[Seq[Int]]].arbitrary.map(_.view.map(_.abs % 2).mkString)

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] = Arbitrary(
    Gen.choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay).map(LocalDate.ofEpochDay),
  )
  /** Bound between the epoch day and now. */
  implicit val arbitraryLocalDateBound: Arbitrary[LocalDate] =
    Arbitrary(Gen.choose(0, LocalDate.now().toEpochDay).map(LocalDate.ofEpochDay))
}
