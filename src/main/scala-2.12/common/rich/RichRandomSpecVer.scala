package common.rich

import scala.collection.IndexedSeqLike
import scala.util.Random

object RichRandomSpecVer {
  implicit class richRandomSpecVer($ : Random) {
    def select[A](xs: IndexedSeqLike[A, _]): A = xs.apply($.nextInt(xs.length))
  }
}
