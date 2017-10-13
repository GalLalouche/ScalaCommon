package common.rich

/** Mostly for adding maps and stuff. */
object RichTuple {
  implicit class richTuple[T]($: (T, T)) {
    def map[S](f: T ⇒ S): (S, S) = (f($._1), f($._2))
    def foreach(f: T ⇒ Unit) {f($._1); f($._2)}
    def toList = List($._1, $._2)
  }

  implicit class richTupleSeqs[T, S]($: (Seq[T], Seq[S])) {
    def zip: Seq[(T, S)] = $._1.zip($._2)
  }
}
