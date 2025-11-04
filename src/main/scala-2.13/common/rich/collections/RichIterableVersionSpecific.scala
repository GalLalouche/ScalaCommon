package common.rich.collections

private object RichIterableVersionSpecific {
  def lengthCompare($ : Iterable[_], n: Int): Int = {
    val knownSize = $.knownSize
    if (knownSize != -1) knownSize.compareTo(n) else RichIterableAux.lengthCompare($, n)
  }
}
