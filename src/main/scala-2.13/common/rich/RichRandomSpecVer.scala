package common.rich

import scala.collection.IndexedSeqOps
import scala.util.Random

object RichRandomSpecVer {
  implicit class richRandomSpecVer($ : Random) {
    def select[A](xs: IndexedSeqOps[A, Any, _]): A = xs.apply($.nextInt(xs.length))
  }
}
