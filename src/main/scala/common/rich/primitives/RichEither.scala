package common.rich.primitives

import cats.implicits.toBifunctorOps

object RichEither {
  implicit class richEither[A, B](private val $ : Either[A, B]) extends AnyVal {
    def eitherStrengthenLeft[C](c: C): Either[(A, C), (B, C)] = $.bimap(_ -> c, _ -> c)
    def eitherStrengthenRight[C](c: C): Either[(C, A), (C, B)] = $.bimap(c -> _, c -> _)
    def fromRight(f: A => B): B = $.fold(f, identity)
  }

  implicit class richEitherString[B](private val $ : Either[String, B]) extends AnyVal {
    def getOrThrow: B = $.fromRight(s => throw new NoSuchElementException(s))
  }
  implicit class richEitherThrowable[B](private val $ : Either[Throwable, B]) extends AnyVal {
    def getOrThrow: B = $.fromRight(throw _)
  }
}
