package common.rich

object RichT {
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
    class __mapper private[RichT] (e: T, p: T => Boolean) {
      def to(f: T => T): T = if (p(e)) f(e) else e
    }

    /** Returns this if the condition is true; if not, returns the default value */
    def onlyIf(b: Boolean): T = (
      if (b) $
      else $ match {
        case _: Int    => 0
        case _: Double => 0.0
        case _: String => ""
      }).asInstanceOf[T]
    def onlyIfNot(b: Boolean): T = onlyIf(!b)

    /** Apply some function to this and returns this. Side effects galore! */
    def applyAndReturn(f: T => Any): T = { f($); $ }

    /** the simple class name, without $ and stuff */
    def simpleName = $.getClass.getSimpleName.replaceAll("\\$", "")

    /** If this is of type C, returns Some(T), else None */
    def safeCast[C](implicit m: Manifest[C]): Option[C] = {
      if (m.runtimeClass.isAssignableFrom($.getClass)) Some($.asInstanceOf[C]) else None
    }
    
    def const: Any => T = e => $
  }
}
