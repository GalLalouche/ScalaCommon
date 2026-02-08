package common.rich

import org.scalacheck.Gen
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

import common.Percentage
import common.rich.RichRandom.richRandom
import common.rich.collections.RichMap.richMap
import common.test.AuxSpecs

class RichRandomTest extends AnyPropSpec with AuxSpecs with ScalaCheckPropertyChecks {
  private val epsilon = 0.01
  private implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

  property("flipCoin") {
    forAll { d: Percentage =>
      val r = new Random()
      val n = 100000
      Vector.fill(n)(r.flipCoin(d)).count(identity).toDouble / n should ===(d.p)
    }
  }

  property("select (int)") {
    val r = new Random()
    val options = Vector(
      1 -> "a",
      2 -> "b",
      1 -> "c",
      3 -> "d",
      0 -> "e",
    )
    val n = 100000
    val m =
      Vector.fill(n)(r.selectW(options)).groupBy(identity).properMapValues(_.size.toDouble / n)
    m("a") should ===(1 / 7.0)
    m("b") should ===(2 / 7.0)
    m("c") should ===(1 / 7.0)
    m("d") should ===(3 / 7.0)
  }

  property("select (double)") {
    val r = new Random()
    val options = Vector(
      0.2 -> "a",
      0.1 -> "b",
      0.2 -> "c",
      0.4 -> "d",
      0.0 -> "e",
    )
    val n = 100000
    val m =
      Vector.fill(n)(r.selectW(options)).groupBy(identity).properMapValues(_.size.toDouble / n)
    m("a") should ===(2 / 9.0)
    m("b") should ===(1 / 9.0)
    m("c") should ===(2 / 9.0)
    m("d") should ===(4 / 9.0)
  }

  private implicit val PercentageGen: Gen[Percentage] = for {
    is1 <- Gen.prob(0.01)
    result <- if (is1) Gen.const(Percentage(1.0)) else Gen.double.map(Percentage(_))
  } yield result
}
