package common

class NGramBuilder[T](freqs: Map[Seq[T], Map[T, Int]]) {
	def predictionRates(count: Int): Double = {
		freqs.map { f =>
			val sum = f._2.values.sum
			val best = f._2.values.max
			val p = best / sum.toDouble
			val weight = sum
			weight * p
		}.toSeq
			.sorted
			.reverse
			.take(count)
			.sum
	}
}

object NGramBuilder {
	def build[T](n: Int, xs: Seq[T]): NGramBuilder[T] = new NGramBuilder[T](xs.sliding(n + 1)
		.foldLeft(Map[Seq[T], Map[T, Int]]())((agg, x) => {
		val key = x.dropRight(1)
		val currentPredictionsForKey = agg.getOrElse(key, Map[T, Int]())
		val value = x.last
		agg.updated(key, currentPredictionsForKey.updated(value, currentPredictionsForKey.getOrElse(value, 0) + 1))
	}))
}
