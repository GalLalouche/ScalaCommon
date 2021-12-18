package common.rich

import scala.util.Random

object RichRandom {
  implicit class richRandom($: Random) {
    def flipCoin(probabilityForTrue: Double): Boolean = $.nextDouble() <= probabilityForTrue
  }
}
