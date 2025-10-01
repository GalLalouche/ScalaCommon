package common.rich

import scala.Ordering.Implicits._
import scala.util.Try

import common.rich.primitives.RichBoolean._

object RichT {
  private val PrimitiveMappings: Map[Class[_], Class[_]] = Map(
    java.lang.Integer.TYPE -> classOf[java.lang.Integer],
    java.lang.Long.TYPE -> classOf[java.lang.Long],
    java.lang.Double.TYPE -> classOf[java.lang.Double],
    java.lang.Float.TYPE -> classOf[java.lang.Float],
    java.lang.Boolean.TYPE -> classOf[java.lang.Boolean],
    java.lang.Character.TYPE -> classOf[java.lang.Character],
    java.lang.Byte.TYPE -> classOf[java.lang.Byte],
    java.lang.Void.TYPE -> classOf[java.lang.Void],
    java.lang.Short.TYPE -> classOf[java.lang.Short],
  )

  // A nice little syntax helper: this allows us to use structures such as mapIf(p).to(something)
  // This is used instead of a local class for performance.
  class __Mapper[T] private[RichT] (e: T, p: T => Boolean) {
    @inline def to(f: T => T): T = if (p(e)) f(e) else e
    @inline def to(t: => T): T = if (p(e)) t else e
  }
  class __Thrower[T] private[RichT] ($ : T, b: Boolean) {
    def thenThrow(f: T => Throwable): T = if (b) $ else throw f($)
    def thenThrow(e: => Throwable): T = if (b) $ else throw e
  }

  implicit class richT[T](private val $ : T) extends AnyVal {
    /**
     * Logs the given string to console and returns this.
     *
     * @param f
     *   An optional stringifier to use; by default, .toString is used
     */
    @inline def log(f: T => Any = x => x.toString): T = applyAndReturn(e => println(f(e)))
    /** Converts this into an Option; this also works for null, beautifully enough :D */
    @inline def opt: Option[T] = Option($)
    @inline def optFilter(p: T => Boolean): Option[T] =
      if ($ == null || p($).isFalse) None else Some($)
    @inline def optMap[S](p: T => Boolean, f: T => S): Option[S] = optFilter(p).map(f)

    @inline def joinOption[S](o: Option[S])(f: (T, S) => T): T = o.fold($)(f($, _))
    /** When you really want a fluent API. */
    @inline def thrush[S](f: T => S): S = f($)
    /** For the F# lovers in the audience. */
    @inline def |>[S](f: T => S): S = thrush(f)

    @inline def mapIf(p: T => Boolean): __Mapper[T] = new __Mapper[T]($, p)
    @inline def mapIf(p: Boolean): __Mapper[T] = new __Mapper[T]($, p.const)

    /** Apply some function to this and returns this. Side effects galore! */
    @inline def applyAndReturn(f: T => Any): T = {
      f($)
      $
    }
    @inline def <|(f: T => Any): T = applyAndReturn(f)

    @inline def :->[S](f: T => S): (T, S) = $ -> f($)
    @inline def <-:[S](f: T => S): (S, T) = f($) -> $

    /** The simple class name, without $ and stuff. */
    @inline def simpleName: String = $.getClass.getSimpleName.replaceAll("\\$", "")

    /** If this is of type C, returns Some(T), else None. */
    def safeCast[C <: T](implicit m: Manifest[C]): Option[C] = {
      val rtc = m.runtimeClass
      // Due to scala's own "primitives", e.g., Int vs int, there is a mismatch between the Manifest[B] and
      // the actual type of RichT. Therefore, we map the primitive classes to their boxed types.
      val referenceClass = PrimitiveMappings.getOrElse(rtc, rtc)
      if (referenceClass.isAssignableFrom($.getClass)) Some($.asInstanceOf[C]) else None
    }

    /** If this is of type C, returns Some(T), else None. */
    def safeAs[C: IsATrait](implicit m: Manifest[C]): Option[C] = $ match {
      case c: C => Some(c)
      case _ => None
    }

    @inline def toTuple[S1, S2](f1: T => S1, f2: T => S2): (S1, S2) = (f1($), f2($))
    @inline def toTuple[S1, S2, S3](f1: T => S1, f2: T => S2, f3: T => S3): (S1, S2, S3) =
      (f1($), f2($), f3($))
    @inline def toTuple[S1, S2, S3, S4](
        f1: T => S1,
        f2: T => S2,
        f3: T => S3,
        f4: T => S4,
    ): (S1, S2, S3, S4) =
      (f1($), f2($), f3($), f4($))
    @inline def toTuple[S1, S2, S3, S4, S5](
        f1: T => S1,
        f2: T => S2,
        f3: T => S3,
        f4: T => S4,
        f5: T => S5,
    ): (S1, S2, S3, S4, S5) =
      (f1($), f2($), f3($), f4($), f5($))

    @inline def tryOrKeep(f: T => T): T = Try(f($)).getOrElse($)
    @inline def optionOrKeep(f: T => Option[T]): T = f($).getOrElse($)

    def coerceIn(min: T, max: T)(implicit o: Ordering[T]): T =
      if ($ < min) min else if ($ > max) max else $

    def ifNot(p: T => Boolean) = new __Thrower($, p($))
    def ifNot(b: Boolean) = new __Thrower($, b)

    /** Similar to ensuring, but throws an IllegalArgumentException instead. */
    @inline def requiring(p: T => Boolean): T = {
      require(p($))
      $
    }
    /** Similar to ensuring, but throws an IllegalArgumentException instead. */
    @inline def requiring(p: T => Boolean, msg: => String): T = {
      require(p($), msg)
      $
    }
  }

  implicit class lazyT[T]($ : => T) {
    def const[S]: S => T = _ => $
    def const2[R, S]: (R, S) => T = (_, _) => $
    def partialConst[S]: PartialFunction[S, T] = { case _ => $ }

    /** Returns Some($) if the condition is true; if not, returns None. */
    def onlyIf(b: Boolean): Option[T] = if (b) Some($) else None
    def onlyIfNot(b: Boolean): Option[T] = onlyIf(b.isFalse)
  }

  implicit class anyRefT[T <: AnyRef](private val $ : T) extends AnyVal {
    @inline def neq(other: T): Boolean = ! $.eq(other)
  }

  trait IsATrait[A]
  object IsATrait {
    import scala.language.experimental.macros
    import scala.reflect.macros.whitebox

    implicit def materialize[A]: IsATrait[A] = macro impl[A]

    def impl[A: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
      import c.universe._
      val tpA = weakTypeOf[A]
      val ts = tpA.typeSymbol.asClass
      if (ts.isTrait) q"new IsATrait[$tpA] {}"
      else c.abort(c.enclosingPosition, s"$tpA is not a trait")
    }
  }
}
