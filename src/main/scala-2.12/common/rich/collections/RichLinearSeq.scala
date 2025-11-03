package common.rich.collections

import scala.collection.{LinearSeq, LinearSeqLike}

object RichLinearSeq {
  implicit class richLinearSeq[CC[X] <: LinearSeqLike[X, CC[X]] with LinearSeq[X], A](
      private val $ : CC[A],
  ) extends AnyVal {
    def headTailOption: Option[(A, CC[A])] = $.headOption.map(_ -> $.tail)
  }
}
