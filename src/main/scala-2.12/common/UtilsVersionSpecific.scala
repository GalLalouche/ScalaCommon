package common

import scala.collection.compat.immutable.ArraySeq

private object UtilsVersionSpecific {
  @inline def unsafeArray[A](a: Array[A]): Seq[A] = ArraySeq.unsafeWrapArray(a)
  @inline def using[R <: AutoCloseable, A](resource: R)(body: R => A): A =
    try
      body(resource)
    finally
      resource.close()
  @inline def lazySeqIterate[A](start: => A)(f: A => A): Seq[A] = Stream.iterate(start)(f)
}
