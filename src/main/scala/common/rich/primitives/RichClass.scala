package common.rich.primitives

object RichClass {
  implicit class richClass[A](private val $: Class[A]) extends AnyVal {
    def isAssignableTo(other: Class[_]): Boolean = other isAssignableFrom $
  }
}
