package common.rich.collections

import common.rich.primitives.RichBoolean.richBoolean
import common.rich.RichT.richT

private object RichIterableVersionSpecific {
  def lengthCompare($: Iterable[_], n: Int): Int = RichIterableAux.lengthCompare($, n)
}
