package common.rich.primitives

import common.rich.collections.RichIterator.richIterator
import common.rich.primitives.RichString.richString
import common.rich.RichT.lazyT

object StringAlgorithms {
  def longestCommonSuffix(xs: Iterable[String]): String = {
    val iter = xs.iterator
    if (iter.isEmpty)
      ""
    else
      iter.lazyFoldl(iter.next())((s, agg) => s.longestCommonSuffix(agg).onlyIf(agg.nonEmpty))
  }
}
