package common.rich.primitives

object RichBoolean {
  implicit class richBoolean(b: Boolean) {
    def ifTrue[T](t: => T): Option[T] = if (b) Some(t) else None
    def ifFalse[T](t: => T): Option[T] = richBoolean(!b).ifTrue(t)
  }
}
