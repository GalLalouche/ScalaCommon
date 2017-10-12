package common.rich

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

  implicit class richT[T]($: T) {
    /**
     * Logs the given string to console and returns this.
     * @param f An optional stringifier to use; by default, .toString is used
     */
    def log(f: T => Any = x => x.toString): T = applyAndReturn { e => println(f(e)) }
    /** Converts this into an Option; this also works for null, beautifully enough :D */
    def opt = Option($)
    /** When you really want a fluent API. */
    def mapTo[S](f: T => S): S = f($)
    /** For the F# lovers in the audience. */
    def |>[S](f: T => S): S = mapTo(f)

    def mapIf(p: T => Boolean): __mapper = new __mapper($, p)

    // A nice little syntax helper: this allows us to use structures such as mapIf(p).to(something)
    // This is used instead of a local class for performance.
    class __mapper private[RichT](e: T, p: T => Boolean) {
      def to(f: T => T): T = if (p(e)) f(e) else e
      def to(t: => T): T = if (p(e)) t else e
    }

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
    def onlyIfNot(b: Boolean): T = onlyIf(!b)

    /** Apply some function to this and returns this. Side effects galore! */
    def applyAndReturn(f: T => Any): T = {
      f($)
      $
    }

    /** the simple class name, without $ and stuff */
    def simpleName = $.getClass.getSimpleName.replaceAll("\\$", "")

    /** If this is of type C, returns Some(T), else None */
    def safeCast[C <: T](implicit m: Manifest[C]): Option[C] = {
      val rtc = m.runtimeClass
      // Due to scala's own "primitives", e.g., Int vs int, there is a mismatch between the Manifest[B] and
      // the actual type of RichT. Therefore, we map the primitive classes to their boxed types.
      val referenceClass = primitiveMappings.getOrElse(rtc, rtc)
      if (referenceClass.isAssignableFrom($.getClass)) Some($.asInstanceOf[C]) else None
    }
  }

  implicit class lazyT[T]($: => T) {
    def const[S]: S => T = _ => $
    def partialConst[S]: PartialFunction[S, T] = { case _ => $ }
  }
}
