package common.rich.primitives

import common.rich.func.{RichOptionT, ToMoreFoldableOps}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

import scalaz.{Applicative, OptionT}
import scalaz.std.OptionInstances

object RichOption extends ToMoreFoldableOps with OptionInstances {
  implicit class richOption[A](private val $: Option[A]) extends AnyVal {
    // throws a better detailed exception when trying to access None
    def getOrThrow(errorMessage: String): A = getOrThrow(new NoSuchElementException(errorMessage))
    def getOrThrow(t: => Throwable): A = toTry(t).get
    def toTry(t: => Throwable): Try[A] = $.mapHeadOrElse(Success.apply, Failure(t))
    def transformer[F[_]: Applicative]: OptionT[F, A] = RichOptionT.app[F].apply($)
  }
}
