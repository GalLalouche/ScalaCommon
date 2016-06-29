package common.rich.collections

// supports containment operators
class RichSet[T]($: Set[T]) {
  def <=[U >: T](other: Set[U]): Boolean = $ forall other.contains
  def <(other: Set[T]): Boolean = <=(other) && $.size < other.size
  def >=(other: Set[T]): Boolean = new RichSet(other) <= $
  def >(other: Set[T]): Boolean = new RichSet(other) < $

  // difference
  def \(other: Set[T]): Set[T] = $ diff other
  def isDisjointTo(other: Set[T]): Boolean = ($ exists other.contains) == false
}

object RichSet {
  implicit def richSet[T]($: Set[T]) = new RichSet($)
}
