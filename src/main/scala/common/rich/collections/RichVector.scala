package common.rich.collections

import java.lang.Math._

/** Like, an algebraic Vector over R^n */
object RichVector {
  implicit class rich($: Vector[Double]) {

    lazy val magnitude: Double = sqrt(dot($))
    private def operatorMap(other: Vector[Double], f: (Double, Double) => Double): Vector[Double] = {
      $.zip({
        require($.size == other.size)
        other
      }).map(e => f(e._1, e._2))
    }
    /** dot product of two vectors */
    def dot(other: Vector[Double]): Double = operatorMap(other, _ * _).sum
    def *(scalar: Double): Vector[Double] = $ map (_ * scalar)
    def ::*(scalar: Double): Vector[Double] = *(scalar)
    def +(other: Vector[Double]): Vector[Double] = operatorMap(other, _ + _)
    def -(other: Vector[Double]): Vector[Double] = operatorMap(other, _ - _)

    /**
     * Calculates the vector that is the difference between this vector and its projection on another vector.
     * The resulting vector is orthogonal to the other vector
     * @param other the other vector
     */
    def antiVector(other: Vector[Double]): Vector[Double] = this.-(this.projectionOn(other))
    
    /** The unit vector, i.e., same direction, but magnitude of 1.0 */
    lazy val toUnit: Vector[Double] = $.map(_ / magnitude)
    
    /** Calculates the projection of this vector on another vector. The resulting vector is parallel to the other vector. */
    def projectionOn(other: Vector[Double]): Vector[Double] = {
      val otherUnit = other.toUnit
      otherUnit.*(this.dot(otherUnit))
    }
    
    def cosineSimilarityTo(other: Vector[Double]): Double = abs(dot(other) / (magnitude * other.magnitude))

  }

}
