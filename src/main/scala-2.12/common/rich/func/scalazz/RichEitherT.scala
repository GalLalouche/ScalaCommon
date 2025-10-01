package common.rich.func.scalazz

import scalaz.{\/-, EitherT, Monad}

object RichEitherT {
  implicit class richEitherT[F[_], E, A](private val $ : EitherT[F, E, A]) extends AnyVal {
    // This already exists in 7.3x, hence why it's only in the 2.12 src code, so this is just copy-pasted from
    // that implementation.
    /** Map on the right of this disjunction. */
    def mapF[B](f: A => F[B])(implicit M: Monad[F]): EitherT[F, E, B] =
      $.flatMapF(f.andThen(M.map(_)(\/-.apply)))
  }
}
