package common.rich.collections

private[collections] class ParIterator[T]($: Iterator[T], windowSize: Int) extends Iterator[T] {
  override val seq = $
  override def hasNext = $.hasNext
  override def next() = $.next()

  override def map[U](f: T => U): Iterator[U] = 
    ($.take(windowSize).toVector.par map f).iterator ++ (if ($.hasNext) new ParIterator($, windowSize) map f else Iterator.empty)

  override def foreach[U](f: T => U) {
    while (hasNext)
      $.take(windowSize).toVector.par foreach f
  }
}
