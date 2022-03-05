package common.rich

/** Mostly for adding maps and stuff. */
object RichTuple {
  implicit class richSameTuple2[A](private val $: (A, A)) extends AnyVal {
    def map[B](f: A => B): (B, B) = f($._1) -> f($._2)
  }
  implicit class richTuple2[A, B](private val $: (A, B)) extends AnyVal {
    @deprecated("use scalaz's fold")
    def reduce[C](f: (A, B) => C): C = f($._1, $._2)
    def modifySecond[C](f: B => C): (A, C) = $._1 -> f($._2)
    def modifyFirst[C](f: A => C): (C, B) = f($._1) -> $._2
  }
  implicit class RightTuple[A, B, C](private val $: (A, (B, C))) extends AnyVal {
    def flatten: (A, B, C) = ($._1, $._2._1, $._2._2)
  }
  implicit class LeftTuple[A, B, C](private val $: ((A, B), C)) extends AnyVal {
    def flatten: (A, B, C) = ($._1._1, $._1._2, $._2)
  }
}
