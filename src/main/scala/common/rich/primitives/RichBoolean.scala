package common.rich.primitives

object RichBoolean {
  implicit class richBoolean(private val b: Boolean) extends AnyVal {
    def ifTrue[T](t: => T): Option[T] = if (b) Some(t) else None
    def ifFalse[T](t: => T): Option[T] = isFalse ifTrue t
    @inline
    def isFalse: Boolean = !b
  }
}
