package common.rich

import scala.util.Random

object RichRandom {
  implicit class richRandom(private val $ : Random) extends AnyVal {
    def flipCoin(probabilityForTrue: Double): Boolean = $.nextDouble() <= probabilityForTrue
  }
}
