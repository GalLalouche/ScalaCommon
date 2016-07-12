package common.rich.collections

// supports containment operators
object RichSet {
  implicit class Rich[T]($: Set[T]) {
    def <=[U >: T](other: Set[U]): Boolean = $ forall other.contains
    def <(other: Set[T]): Boolean = <=(other) && $.size < other.size
    def >=(other: Set[T]): Boolean = new Rich(other) <= $
    def >(other: Set[T]): Boolean = new Rich(other) < $
    // difference
    def \(other: Set[T]): Set[T] = $ diff other
    def isDisjointTo(other: Set[T]): Boolean = false == $.exists(other.contains)
  }
}
