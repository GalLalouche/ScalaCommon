package common.rich.primitives

import scala.util.{Failure, Success, Try}

object RichOption {
  implicit class richOption[T]($: Option[T]) {
    // throws a better detailed exception when trying to access None
    def getOrThrow(errorMessage: String): T = getOrThrow(new NoSuchElementException(errorMessage))
    def getOrThrow(t: => Throwable): T = toTry(t).get
    def toTry(t: => Throwable): Try[T] = mapOrElse(Success.apply, Failure(t))
    // Saner order, and doesn't have problems with type inference.
    def mapOrElse[S](f: T => S, default: => S): S = $.fold(default)(f)
    // Alias for mapOrElse
    def foldR[S](f: T => S, default: => S): S = mapOrElse(f, default)
  }
}
