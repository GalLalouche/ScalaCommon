package common.test

import alleycats.std.all.alleycatsStdIterableTraverse
import cats.implicits.toTraverseOps
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.util.Buildable

import common.test.kats.GenInstances.MonadGen

object MoreGen {
  def prefix(s: String, minSize: Int = 0): Gen[String] = Gen.choose(minSize, s.length).map(s.take)
  /** Generates a randomly capitalized version of s. Useful for tresting case insensitivity. */
  def capitalization(s: String): Gen[String] =
    (s: Iterable[Char]).traverse(c => Gen.oneOf(c.toLower, c.toUpper)).map(_.mkString)
  /** More efficient than [[Gen#oneOf(Seq)]] if the [[IndexedSeq]] isn't a [[Vector]]. */
  def oneOf[A](xs: IndexedSeq[A]): Gen[A] = Gen.choose(0, xs.size - 1).map(xs.apply)
  val nonEmptyAlphaNumString: Gen[String] = nonEmptyString(Gen.alphaNumChar)
  def nonEmptyString(c: => Gen[Char]): Gen[String] = Gen.nonEmptyListOf(c).map(_.mkString)

  /**
   * For example, to create a [[Vector]] of 0 to 10 [[String]]s:
   * {{{
   *   containerOfN[Vector, String](Gen.choose(0, 10))
   * }}}
   */
  def containerOfN[CC[_], A: Arbitrary](n: Gen[Int])(implicit
      evb: Buildable[A, CC[A]],
      evt: CC[A] => Iterable[A],
  ): Gen[CC[A]] = n.flatMap(Gen.containerOfN[CC, A](_, arbitrary[A]))
}
