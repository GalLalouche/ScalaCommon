package common

// In 2.12, WrappedArray is in mutable, but mutable.Seq extends Seq, so we're fine.
import scala.collection.mutable

private object UtilsVersionSpecific {
  @inline def unsafeArray[A](a: Array[A]): Seq[A] = mutable.WrappedArray.make(a)
}
