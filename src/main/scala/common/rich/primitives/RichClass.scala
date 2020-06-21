package common.rich.primitives

import scala.reflect.ClassTag

object RichClass {
  implicit class richClass[A](private val $: Class[A]) extends AnyVal {
    def isAssignableTo(other: Class[_]): Boolean = other isAssignableFrom $
  }

  implicit class richClassTag[A](private val ct: ClassTag[A]) extends AnyVal {
    def unerasedClass: Class[A] = ct.runtimeClass.asInstanceOf[Class[A]]
  }
}
