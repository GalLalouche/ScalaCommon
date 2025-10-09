package common.rich.collections

import scala.collection.SeqView

object RichSeqView {
  implicit class richSeqView[A](private val $ : SeqView[A]) extends AnyVal {
    // sortBy always returns a SeqView, but the type checker doesn't know that ¯\_(ツ)_/¯
    def castSortBy[B: Ordering](f: A => B): SeqView[A] = $.sortBy(f).asInstanceOf[SeqView[A]]
    def lift(n: Int): Option[A] = {
      if (n < 0)
        return None
      // noinspection IndexBoundsCheck (scala plugin bug, there is no lift, hence this method!)
      if ($.knownSize != -1)
        return if (n < $.length) Some($.apply(n)) else None

      // If the size isn't known, then iterating over the collection would probably just as
      // efficient as apply anyway!
      var i = 0
      val iterator = $.iterator
      while (iterator.hasNext) {
        val next = iterator.next()
        if (i == n)
          return Some(next)
        else
          i += 1
      }
      None
    }
  }
}
