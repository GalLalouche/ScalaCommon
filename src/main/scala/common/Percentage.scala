package common

import scala.language.implicitConversions
import scala.util.Random

import common.rich.primitives.RichDouble._

class Percentage private (val p: Double) extends AnyVal {
  def >=(d: Double): Boolean = p >= d
  def <=(d: Double): Boolean = p <= d

  def *(e: Int): Int = math.round(p * e).toInt
  def *(e: Long): Long = math.round(p * e)
  def *(e: Double): Double = p * e
  def *(e: Float): Double = p * e
  override def toString: String = p.withPrecision(2)
  /** Returns true with probability p. */
  def roll(r: Random): Boolean = r.nextDouble() <= p
  def inverse: Percentage = new Percentage(1 - p)
  def prettyPrint(decimalDigits: Int): String = s"${(100 * p).withPrecision(decimalDigits)}%"
}

object Percentage {
  def conform(p: Double): Percentage =
    if (p <= 0) Percentage(0.0) else if (p >= 1) Percentage(1.0) else p

  // One might wonder what's the point of an implicit def in this case, since it forgoes all type
  // safety. The answer is laziness (of course), and Percentage still enjoys the added benefit of
  // bound checking at runtime, as well as revealing intent to clients.
  implicit def toPercentage(d: Double): Percentage = Percentage(d)
  def apply(x: Double, y: Double): Percentage = apply(x / y)
  def apply(p: Double): Percentage = {
    require(p >= 0 && p <= 1)
    new Percentage(p)
  }

  implicit val orderingEv: Ordering[Percentage] = Ordering.by(_.p)

  /**
   * Equivalent to {{{0.0 to 1.0 by step}}}. This method is needed since it's possible to get
   * numbers which are larger than 1.0 due to floating point errors. The last number in returned
   * sequence is guaranteed to always be 1.0.
   */
  def step(by: Percentage): Seq[Percentage] = {
    val p = by.p
    require(p > 0 && p < 0.5)
    (BigDecimal(0.0) to BigDecimal(1.0) by BigDecimal(p))
      .map(bd => Math.min(1, bd.doubleValue))
      .map(Percentage.apply)
  }
}
