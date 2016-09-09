package common.rich.primitives

object RichEither {
  implicit class richEither[A, B]($: Either[A, B]) {
    def resolve[C](f: A => C, g: B => C): C = $ match {
      case Left(a) => f(a)
      case Right(b) => g(b)
    }
  }
}
