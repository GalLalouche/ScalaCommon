package common.rich.primitives

import scala.util.{Failure, Success, Try}

object RichTry {
  implicit class richTry[A](private val $: Try[A]) extends AnyVal {
    def getOrElseF(f: Throwable => A): A = $ match {
      case Failure(exception) => f(exception)
      case Success(value) => value
    }
  }
}

