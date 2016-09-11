package common.rich.primitives

object RichOption {
	implicit class Rich[T]($: Option[T]) {
		// throws a better detailed exception when trying to access None
		def getOrThrow(errorMessage: String): T = $ match {
			case None => throw new NoSuchElementException(errorMessage)
			case Some(e) => e
		}
		def either[S](b: => S): Either[S, T] = Either.cond($.isDefined, $.get, b)
	}
}
