package common.rich.primitives

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

import scalaz.std.option.optionInstance
import common.rich.func.ToMoreFoldableOps._

object RichOption {
  implicit class richOption[A](private val $: Option[A]) extends AnyVal {
    // throws a better detailed exception when trying to access None
    def getOrThrow(errorMessage: => String): A = getOrThrow(new NoSuchElementException(errorMessage))
    def getOrThrow(t: => Throwable)(implicit d: DummyImplicit): A = toTry(t).get
    def toTry(t: => Throwable): Try[A] = $.mapHeadOrElse(Success.apply, Failure(t))
  }
}
