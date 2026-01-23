package common.rich.primitives

import cats.implicits.toBifunctorOps

import scala.util.{Failure, Success, Try}

object RichEither {
  implicit class richEither[A, B](private val $ : Either[A, B]) extends AnyVal {
    def eitherStrengthenLeft[C](c: C): Either[(A, C), (B, C)] = $.bimap(_ -> c, _ -> c)
    def eitherStrengthenRight[C](c: C): Either[(C, A), (C, B)] = $.bimap(c -> _, c -> _)
    def fromRight(f: A => B): B = $.fold(f, identity)
    def getOrThrow(implicit ev: ToError[A]): B = $.fromRight(a => throw ev.toError(a))
    def toErrorTry(implicit ev: ToError[A]): Try[B] = $ match {
      case Left(a) => Failure(ev.toError(a))
      case Right(b) => Success(b)
    }
  }

  trait ToError[E] {
    def toError(e: E): Throwable
  }
  object ToError {
    implicit def throwableError[E <: Throwable]: ToError[E] = identity
    /**
     * This implementation isn't implicit since not all types' `toString` is a meaningful error
     * message. It can be used to make it explicit when it is. Example usage:
     * {{{
     * sealed trait SomeError
     * object SomeError {
     *   case object Error1 extends SomeError
     *   case object Error2 extends SomeError
     *   implicit val toError: ToError[SomeError] = ToError.fromToString[SomeError]
     * }
     * }}}
     */
    def fromToString[A]: ToError[A] = e => new NoSuchElementException(e.toString)
    /**
     * [[String]] isn't a very typeful error, but it seems to be pretty idiomatic, so might as well.
     */
    implicit object StringError extends ToError[String] {
      override def toError(s: String): Throwable = new NoSuchElementException(s)
    }
  }
}
