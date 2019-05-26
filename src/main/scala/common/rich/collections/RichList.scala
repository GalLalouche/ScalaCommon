package common.rich.collections

object RichList {
  implicit class richList[A](private val $: List[A]) extends AnyVal {
    def headTailOption: Option[(A, List[A])] = $ match {
      case Nil => None
      case ::(head, tail) => Some(head -> tail)
    }
  }
}
