package common.rich.collections

import common.rich.RichT.richT
import common.rich.primitives.RichBoolean.richBoolean

private object RichIterableVersionSpecific {
  def lengthCompare($ : Iterable[_], n: Int): Int = {
    val knownSize = $.knownSize
    if (knownSize != -1) knownSize.compareTo(n) else RichIterableAux.lengthCompare($, n)
  }
}
