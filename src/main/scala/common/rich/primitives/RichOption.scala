package common.rich.primitives

import alleycats.Empty

import scala.util.{Failure, Success, Try}

import common.rich.func.kats.ToMoreFoldableOps.toMoreFoldableOps

object RichOption {
  implicit class richOption[A](private val $ : Option[A]) extends AnyVal {
    // throws a better detailed exception when trying to access None
    def getOrThrow(errorMessage: => String): A = getOrThrow(
      new NoSuchElementException(errorMessage),
    )
    def getOrThrow(t: => Throwable)(implicit d: DummyImplicit): A = toTry(t).get
    /** Unlike `orEmpty` in cats [[cats.syntax.OptionOps]], requires only [[Empty]] */
    def getOrEmpty(implicit E: Empty[A]): A = $.getOrElse(E.empty)
    def toTry(t: => Throwable): Try[A] = $.mapHeadOrElse(Success.apply, Failure(t))
    // Since for some stupid reason $.toIterable is deprecated.
    def asIterable: Iterable[A] = $
    def ifNone(f: => Unit): Unit = if ($.isEmpty) f
  }
}
