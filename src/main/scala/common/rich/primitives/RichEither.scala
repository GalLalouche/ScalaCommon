package common.rich.primitives

import scalaz.std.either.eitherInstance
import scalaz.syntax.bifunctor.ToBifunctorOps

object RichEither {
  implicit class richEither[A, B](private val $ : Either[A, B]) extends AnyVal {
    def eitherStrengthenLeft[C](c: C): Either[(A, C), (B, C)] = $.bimap(_ -> c, _ -> c)
    def eitherStrengthenRight[C](c: C): Either[(C, A), (C, B)] = $.bimap(c -> _, c -> _)
  }
}
