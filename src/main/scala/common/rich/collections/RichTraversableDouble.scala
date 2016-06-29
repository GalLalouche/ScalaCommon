package common.rich.collections

import common.LRUCache

class RichTraversableDouble($: Traversable[Double]) {
	require($ nonEmpty)
	lazy val mean = $.sum / $.size
	lazy val standardDeviation = Math.sqrt(($
		map (_ - mean)
		map (x ⇒ x * x) sum) / $.size)
	lazy val normalizedByMean = ($ map (e ⇒ (e - mean) / standardDeviation)).toSeq
	private lazy val min = $.min
	private lazy val secondMin =
		try $.toSet.toVector.sorted.apply(1)
		catch {case e: IndexOutOfBoundsException => min + 1}
	lazy val rerange2Positives: Seq[Double] = {
		if ($.forall(_ > 0))
			$.toSeq
		else {
			val diff = -min + (secondMin - min)
			($ map (_ + diff)).toVector
		}
	}
	lazy val normalizedByRankings: Seq[Double] = {
		def aux($: Traversable[Double]) = {
			val sortedMap = sorted
				.zipWithIndex
				.groupBy(_ _1)
				.map(e ⇒ (e._1, e._2.map(_._2 + 1).sum / e._2.size.toDouble))
			$.map(e ⇒ sortedMap(e) / $.size.toDouble)
				.toVector
		}
		aux($)
	}
	lazy val magnitude = Math.sqrt($.map(e => e * e).sum)
	lazy val unitVector = $ map (_ / magnitude)
	private lazy val sorted = $.toVector.sorted
	lazy val median = (sorted($.size / 2) + sorted(($.size - 1) / 2)) / 2
	lazy val decimal = sorted($.size / 10)
	lazy val medianAbsoluteDeviation = new RichTraversableDouble($ map (_ - median) map Math.abs).median
	override def toString = $.toString
	def euclideanDistanceFrom(other: Traversable[Double]): Double = {
		require($.size == other.size)
		$.toSeq
			.zip(other.toSeq)
			.map(e => e._1 - e._2)
			.map(e => e * e)
			.sum
	}
	def fixedSizeBins(binSize: Double): Seq[Long] = {
		require(binSize > 0)
		val bins = sorted
			.groupBy(e => (e / binSize).floor.toInt)
			.map(e => e._1 -> e._2.size)
			.toSeq
			.sortBy(_._1)
		val $ = new Array[Long](bins.last._1 + 1)
		for (e <- bins)
			$(e._1) = e._2
		$.toVector
	}

	def scalarProduct(other: Traversable[Double]): Double = {
		require($.size == other.size,
			s"Can't do scalar products with differently sized collections (${$.size}, ${other.size})")
		$.toSeq
			.zip(other.toSeq)
			.map(e => e._1 * e._2)
			.sum
	}

	def range: (Double, Double) = $.min -> $.max // not very effective
	def percentile(d: Double) = {
		require(d >= 0 && d < 1)
		sorted((sorted.size * d).toInt)
	}
}

object RichTraversableDouble {
	private val latest = new LRUCache[Int, RichTraversableDouble](10)
	// caching
	implicit def richTraversableDouble($: Traversable[Double]): RichTraversableDouble = {
		new RichTraversableDouble($)
	}

	implicit def richTraversableInt($: Traversable[Int]): RichTraversableDouble = {
		this.synchronized {
			val hash = System identityHashCode $
			if (latest.contains(hash) == false)
				latest(hash) = new RichTraversableDouble($ map (_.toDouble))
			latest(hash)
		}
	}
}
