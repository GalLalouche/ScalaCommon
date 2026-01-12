package common

import scala.collection.compat.immutable.ArraySeq

private object UtilsVersionSpecific {
  @inline def unsafeArray[A](a: Array[A]): Seq[A] = ArraySeq.unsafeWrapArray(a)
}
