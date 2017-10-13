package common.rich.primitives

import scala.util.{Failure, Success, Try}

object RichOption {
  implicit class richOption[T]($: Option[T]) {
    // throws a better detailed exception when trying to access None
    def getOrThrow(errorMessage: String): T = getOrThrow(new NoSuchElementException(errorMessage))
    def getOrThrow(t: => Throwable): T = toTry(t).get
    def toTry(t: => Throwable): Try[T] = $ match {
      case Some(e) => Success(e)
      case None => Failure(t)
    }
  }
}
