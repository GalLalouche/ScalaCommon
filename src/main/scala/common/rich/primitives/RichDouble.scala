package common.rich.primitives

object RichDouble {
	implicit class Rich(d: Double) {
		def **(d2: Double) = Math.pow(d, d2)
		def sq = d * d
		def sqrt = Math sqrt d
		def isInt = d - d.toInt == 0
		def inAngles = d * 180 / Math.PI
		def isRoughly(o: Double, precision: Double = 0.001) = d - o < precision
    /** Checks that this double isn't NaN or one of the infinities. */
		def isRealDouble = false == (d.isNaN || d.isPosInfinity || d.isNegInfinity)
		def shiftedLog = Math.log(d + 1)
    /** For those who cannot for the life of them remember string formatting. */
		def withPrecision(p: Int): String = {
			val $ = f"$d%.10f".split("\\.")
			$(0) + "." + $(1).take(p)
		}
	}
}
