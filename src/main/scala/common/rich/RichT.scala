package common.rich

import common.rich.primitives.RichBoolean._

import scala.util.Try

object RichT {
  private val primitiveMappings: Map[Class[_], Class[_]] = Map(
    java.lang.Integer.TYPE -> classOf[java.lang.Integer],
    java.lang.Long.TYPE -> classOf[java.lang.Long],
    java.lang.Double.TYPE -> classOf[java.lang.Double],
    java.lang.Float.TYPE -> classOf[java.lang.Float],
    java.lang.Boolean.TYPE -> classOf[java.lang.Boolean],
    java.lang.Character.TYPE -> classOf[java.lang.Character],
    java.lang.Byte.TYPE -> classOf[java.lang.Byte],
    java.lang.Void.TYPE -> classOf[java.lang.Void],
    java.lang.Short.TYPE -> classOf[java.lang.Short])

  class __Mapper[T] private[RichT](e: T, p: T => Boolean) {
    def to(f: T => T): T = if (p(e)) f(e) else e
    def to(t: => T): T = if (p(e)) t else e
  }

  implicit class richT[T](private val $: T) extends AnyVal {
    /**
     * Logs the given string to console and returns this.
     * @param f An optional stringifier to use; by default, .toString is used
     */
    def log(f: T => Any = x => x.toString): T = applyAndReturn {e => println(f(e))}
    /** Converts this into an Option; this also works for null, beautifully enough :D */
    def opt = Option($)
    /** When you really want a fluent API. */
    def mapTo[S](f: T => S): S = f($)
    /** For the F# lovers in the audience. */
    def |>[S](f: T => S): S = mapTo(f)

    def mapIf(p: T => Boolean): __Mapper[T] = new __Mapper[T]($, p)
    def mapIf(p: Boolean): __Mapper[T] = new __Mapper[T]($, p.const)

    // A nice little syntax helper: this allows us to use structures such as mapIf(p).to(something)
    // This is used instead of a local class for performance.

    /** Returns this if the condition is true; if not, returns the default value */
    def onlyIf(b: Boolean): T = (
        if (b) $
        else $ match {
          case _: Int => 0
          case _: Long => 0L
          case _: Double => 0.0
          case _: Float => 0.0f
          case _: String => ""
          case _: Any => null
        }).asInstanceOf[T]
    def onlyIfNot(b: Boolean): T = onlyIf(b.isFalse)

    /** Apply some function to this and returns this. Side effects galore! */
    def applyAndReturn(f: T => Any): T = {
      f($)
      $
    }
    def <|(f: T => Any): T = applyAndReturn(f)

    def :->[S](f: T => S): (T, S) = $ -> f($)
    def <-:[S](f: T => S): (S, T) = f($) -> $

    /** the simple class name, without $ and stuff */
    def simpleName: String = $.getClass.getSimpleName.replaceAll("\\$", "")

    /** If this is of type C, returns Some(T), else None */
    def safeCast[C <: T](implicit m: Manifest[C]): Option[C] = {
      val rtc = m.runtimeClass
      // Due to scala's own "primitives", e.g., Int vs int, there is a mismatch between the Manifest[B] and
      // the actual type of RichT. Therefore, we map the primitive classes to their boxed types.
      val referenceClass = primitiveMappings.getOrElse(rtc, rtc)
      if (referenceClass.isAssignableFrom($.getClass)) Some($.asInstanceOf[C]) else None
    }

    def toTuple[S1, S2](f1: T => S1, f2: T => S2): (S1, S2) = (f1($), f2($))
    def toTuple[S1, S2, S3](f1: T => S1, f2: T => S2, f3: T => S3): (S1, S2, S3) = (f1($), f2($), f3($))
    def toTuple[S1, S2, S3, S4](f1: T => S1, f2: T => S2, f3: T => S3, f4: T => S4): (S1, S2, S3, S4) =
      (f1($), f2($), f3($), f4($))

    def tryOrKeep(f: T => T): T = Try(f($)).getOrElse($)
  }

  implicit class lazyT[T]($: => T) {
    def const[S]: S => T = _ => $
    def partialConst[S]: PartialFunction[S, T] = {case _ => $}
  }

  implicit class anyRefT[T <: AnyRef](private val $: T) extends AnyVal {
    def neq(other: T): Boolean = !$.eq(other)
  }
}
