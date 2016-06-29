package common.rich

object RichT {
  // This implicit is used to convert a value or literal into a function that returns that value
  // It is used in functions such as mapIf
  implicit def pure2Generator[S](x: S): Any => S = _ => x
  implicit class richT[T](e: T) {
    /**
     * logs the given string to console and returns this
     * @param f An optional stringifier to use.
     * By default, .toString is used
     */
    def log(f: T => Any = x => x.toString): T = applyAndReturn { e => println(f(e)) }

    /** Converts this into a Option */
    def opt = Option(e)

    def mapTo[S](f: T => S): S = f(e)
    /** For the F# lovers in the audience */
    def |>[S](f: T => S): S = mapTo(f)

    def mapIf(p: T => Boolean): __mapper = new __mapper(e, p)

    // A nice little syntax helper: this allows us to use structures such as mapIf(p).to(something)
    class __mapper private[RichT] (e: T, p: T => Boolean) {
      def to(f: T => T): T = if (p(e)) f(e) else e
    }

    /**
     * Returns this if the condition is true
     * If not, returns the default value
     */
    def onlyIf(b: Boolean): T = (
      if (b) e
      else e match {
        case _: Int    => 0
        case _: Double => 0.0
        case _: String => ""
      }).asInstanceOf[T]
    def onlyIfNot(b: Boolean): T = onlyIf(!b)

    /** Apply some function to this and returns this */
    def applyAndReturn(f: T => Any): T = {
      f(e)
      e
    }

    /** the simple class name, without $ and stuff */
    def simpleName = e.getClass.getSimpleName.replaceAll("\\$", "")

    /** If this is of type T, returns Some(T), else None */
    def safeCast[C](implicit m: Manifest[C]): Option[C] = {
//      println(m.runtimeClass)
//      println(e.getClass)
      if (m.runtimeClass.isAssignableFrom(e.getClass)) Some(e.asInstanceOf[C]) else None
    }
  }
}
