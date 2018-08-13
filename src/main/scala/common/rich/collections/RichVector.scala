package common.rich.collections

import java.lang.Math._
import common.rich.primitives.RichDouble._

/** Like, an algebraic Vector over ℝ^n */
object RichVector {
  type Vektor = Vector[Double]

  implicit class richVector(private val $: Vektor) extends AnyVal {
    def magnitude: Double = sqrt(dot($))

    private def operatorMap(other: Vektor, f: (Double, Double) => Double): Vektor = {
      require($.size == other.size)
      $ zip other map f.tupled
    }

    /** dot product of two vectors */
    def dot(other: Vektor): Double = operatorMap(other, _ * _).sum
    def *(scalar: Double): Vektor = $ map (_ * scalar)
    def ::*(scalar: Double): Vektor = *(scalar)
    def +(other: Vektor): Vektor = operatorMap(other, _ + _)
    def -(other: Vektor): Vektor = operatorMap(other, _ - _)

    /**
     * Calculates the vector that is the difference between this vector and its projection on another vector.
     * The resulting vector is orthogonal to the other vector.
     */
    def antiVector(other: Vektor): Vektor = this.-(projectionOn(other))

    /** The unit vector, i.e., same direction, but has magnitude of 1.0 */
    def toUnit: Vektor = $.map(_ / magnitude)

    /** Calculates the projection of this vector on another vector. The resulting vector is parallel to the other vector. */
    def projectionOn(other: Vektor): Vektor = {
      val otherUnit = other.toUnit
      otherUnit * this.dot(otherUnit)
    }

    def cosineSimilarityTo(other: Vektor): Double = abs(dot(other) / (magnitude * other.magnitude))

    def euclideanDistanceFrom(other: Vektor): Double = this.-(other).map(_.sq).sum
  }
}
