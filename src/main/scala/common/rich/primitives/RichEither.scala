package common.rich.primitives

object RichEither {
  implicit class richEither[A, B](private val $: Either[A, B]) extends AnyVal {
    def resolve[C](f: A => C, g: B => C): C = $ match {
      case Left(a) => f(a)
      case Right(b) => g(b)
    }
  }
}
