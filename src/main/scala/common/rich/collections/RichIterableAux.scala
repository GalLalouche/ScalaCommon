package common.rich.collections

import common.rich.RichT.richT
import common.rich.primitives.RichBoolean.richBoolean

private object RichIterableAux {
  def lengthCompare($ : Iterable[_], n: Int): Int = {
    val i = $.iterator.drop(n - 1)
    if (i.hasNext.isFalse) -1 else if (i.<|(_.next()).hasNext) 1 else 0
  }
}
