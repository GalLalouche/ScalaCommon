package common.rich

import common.rich.RichT.richT

/**
 * Mostly for adding maps and stuff
 */
object RichTuple {
  implicit def richTuple[T]($: (T, T)) = new {
    def map[S](f: T ⇒ S): (S, S) = (f($._1), f($._2))
    def foreach(f: T ⇒ Unit) { f($._1); f($._2) }
    def toList = List($._1, $._2)
  }

  implicit def richTupleSeqs[T, S]($: (Seq[T], Seq[S])) = new {
    def zip: Seq[(T, S)] = $.mapTo(x ⇒ $._1 zip $._2)
  }
}
