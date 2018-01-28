package common.rich.primitives

import common.rich.func.ToMoreFoldableOps

import scala.util.{Failure, Success, Try}
import scalaz.std.OptionInstances

object RichOption {
  implicit class richOption[T]($: Option[T])
      extends ToMoreFoldableOps with OptionInstances {
    // throws a better detailed exception when trying to access None
    def getOrThrow(errorMessage: String): T = getOrThrow(new NoSuchElementException(errorMessage))
    def getOrThrow(t: => Throwable): T = toTry(t).get
    def toTry(t: => Throwable): Try[T] = $.mapHeadOrElse(Success.apply, Failure(t))
  }
}
