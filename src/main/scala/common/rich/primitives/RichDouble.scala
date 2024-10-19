package common.rich.primitives

import common.rich.primitives.RichBoolean._

object RichDouble {
  implicit class richDouble(private val d: Double) extends AnyVal {
    def **(d2: Double): Double = Math.pow(d, d2)
    def sq: Double = d * d
    def sqrt: Double = Math.sqrt(d)
    def isInt: Boolean = d - d.toInt == 0
    def inAngles: Double = d * 180 / Math.PI
    def isRoughly(o: Double, precision: Double = 0.001): Boolean = math.abs(d - o) < precision
    /** Checks that this double isn't NaN or one of the infinities. */
    def isRealDouble: Boolean = (d.isNaN || d.isPosInfinity || d.isNegInfinity).isFalse
    def shiftedLog: Double = Math.log(d + 1)
    /** For those who cannot for the life of them remember string formatting. */
    def withPrecision(p: Int): String = {
      require(p >= 0)
      if (p == 0)
        d.toInt.toString
      else {
        val $ = f"$d%.10f".split("\\.")
        $(0) + "." + $(1).take(p)
      }
    }
  }
}
