package common.rich.collections

object RichTraversableDouble {
  implicit class richTraversableDouble($: Traversable[Double]) {
    require($ nonEmpty)

    def mean = $.sum / $.size

    def standardDeviation = Math.sqrt(($
      map (_ - mean)
      map (x ⇒ x * x) sum) / $.size)

    def normalizedByMean = ($ map (e ⇒ (e - mean) / standardDeviation)).toSeq

    def rerange2Positives: Seq[Double] = {
      val min = $.min
      val secondMin =
        try $.toSet.toVector.sorted.apply(1)
        catch {
          case e: IndexOutOfBoundsException => min + 1
        }
      if ($.forall(_ > 0))
        $.toSeq
      else {
        val diff = -min + (secondMin - min)
        ($ map (_ + diff)).toVector
      }
    }

    def normalizedByRankings: Seq[Double] = {
      val sortedMap = sorted
        .zipWithIndex
        .groupBy(_ _1)
        .map(e ⇒ (e._1, e._2.map(_._2 + 1).sum / e._2.size.toDouble))
      $.map(e ⇒ sortedMap(e) / $.size.toDouble).toVector // why toVector?
    }

    private lazy val sorted = $.toVector.sorted

    def median = (sorted($.size / 2) + sorted(($.size - 1) / 2)) / 2
    def decimal = sorted($.size / 10)
    def medianAbsoluteDeviation = new richTraversableDouble($ map (_ - median) map Math.abs).median

    override def toString = $.toString

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

    def range: (Double, Double) = $.min -> $.max

    // not very effective
    def percentile(d: Double): Double = {
      require(d >= 0 && d < 1)
      sorted((sorted.size * d).toInt)
    }
  }

  implicit def richInt($: Traversable[Int]): richTraversableDouble = new richTraversableDouble($ map (_.toDouble))
}
