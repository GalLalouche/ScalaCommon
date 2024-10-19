package common.rich.primitives

import scala.annotation.tailrec

import common.rich.collections.RichIterator.richIterator
import common.rich.primitives.RichBoolean.richBoolean
import common.rich.primitives.RichString.richString

object StringAlgorithms {
  def longestCommonSuffix(xs: Iterable[String]): String = {
    val iterator = xs.iterator
    @tailrec def go(agg: String): String = {
      if (iterator.hasNext.isFalse || agg == "") agg else go(iterator.next.longestCommonSuffix(agg))
    }
    val next = iterator.headOption()
    next.map(go).getOrElse("")
  }
}
