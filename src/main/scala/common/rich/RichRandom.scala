package common.rich

import scala.collection.{IndexedSeqLike, SeqLike}
import scala.util.Random

object RichRandom {
  implicit class richRandom($: Random) {
    def flipCoin(probabilityForTrue: Double): Boolean = $.nextDouble() <= probabilityForTrue
    def select[A](xs: IndexedSeqLike[A, _]): A = xs.apply($.nextInt(xs.length))
  }
}
