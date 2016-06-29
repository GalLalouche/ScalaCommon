package common.rich.primitives

object RichOption {
	def richOption[T](o: Option[T]) = new {
		// throws a better detailed exception when trying to access None
		def expect(errorMessage: String): T = o match {
			case None => throw new NoSuchElementException(errorMessage)
			case Some(e) => e
		}
	}
}
