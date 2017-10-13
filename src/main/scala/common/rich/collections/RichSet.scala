package common.rich.collections

// supports containment operators
object RichSet {
  implicit class richSet[T]($: Set[T]) {
    def <=[U >: T](other: Set[U]): Boolean = $ forall other.contains
    def <(other: Set[T]): Boolean = <=(other) && $.size < other.size
    def >=(other: Set[T]): Boolean = new richSet(other) <= $
    def >(other: Set[T]): Boolean = new richSet(other) < $
    // difference
    @deprecated("Use &~")
    def \(other: Set[T]): Set[T] = $ &~ other
    def isDisjointTo(other: Set[T]): Boolean = false == $.exists(other.contains)
  }
}
