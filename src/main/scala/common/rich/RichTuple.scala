package common.rich

/** Mostly for adding maps and stuff. */
object RichTuple {
  implicit class richTuple2[A, B](private val $: (A, B)) extends AnyVal {
    def reduce[C](f: (A, B) => C): C = f($._1, $._2)
    def modifySecond[C](f: B => C): (A, C) = $._1 -> f($._2)
    def modifyFirst[C](f: A => C): (C, B) = f($._1) -> $._2
  }
  implicit class richSameTuple2[A](private val $: (A, A)) extends AnyVal {
    def map[S](f: A => S): (S, S) = (f($._1), f($._2))
    def foreach(f: A => Unit) {f($._1); f($._2)}
    def toList = List($._1, $._2)
  }

  implicit class richTupleSeqs[A, B](private val $: (Seq[A], Seq[B])) extends AnyVal {
    def zip: Seq[(A, B)] = $._1.zip($._2)
  }
}
