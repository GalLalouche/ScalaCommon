package common.rich.collections

import scala.util.Try

class RichIterator[T]($: Iterator[T]) {

	/** Returns an iterator that throws an exception on the first item that does not satisfy f */
	def verify(f: T ⇒ Boolean,
						 exceptionMessage: (T, Int) ⇒ String = (e, i) ⇒ s"Item $e @ $i failed f"): Iterator[T] =
		$.zipWithIndex.map(e ⇒ {if (f(e._1) == false) throw new Exception(exceptionMessage(e._1, e._2)) else e._1})

	/**
	 * A try map that outputs nothing
	 * @param f the mapping function
	 */
	def tryMapSilent[U](f: T => U) = $
		.map(e ⇒ (e, Try(f(e))))
		.filter(e ⇒ e._2.isSuccess)
		.map(_._2.get)

	/**
	 * Tries to map this iterator.
	 * It is possible to catch certain exceptions that are thrown during the mapping
	 * functions evaluation.
	 * If an exception is caught, and it is of the correct type, it is logged and the element to be mapped is skipped.
	 * If an exception isn't of the correct type, it is re-thrown and the function terminates.
	 * @param f the mapping function
	 * @param errorMessage the message to write to console when an exception is caught and processed
	 * @param errorType the type of exceptions to catch
	 */
	def tryMap[U](f: T ⇒ U,
								errorMessage: T => String = e => s"Error on $e",
								// throwable, because composite on parallel :\
								errorType: Class[_ <: Throwable] = classOf[Throwable]): Iterator[U] =
		$
			.map(e ⇒ (e, Try(f(e))))
			.filter(e ⇒ {
				if (e._2.isFailure)
					if (errorType.isAssignableFrom(e._2.failed.get.getClass))
						println(errorMessage(e._1) + s"\nFailed with message: '${e._2.failed.get.getMessage}', skipping")
					else
						throw e._2.failed.get
				e._2.isSuccess
			})
			.map(_._2.get)

	/**
	 * Returns an iterator that outputs to the console its iteration number
	 * @param frequency the frequency of the output, i.e., how often should the message
	 * be printed.
	 * Default is every time, i.e., at every step.
	 */
	def withCounter(frequency: Int = 1): Iterator[T] = withCounter(i => if (i % frequency == 0) Some(i.toString) else None)

	/**
	 * Returns an iterator that outputs to the console its progress
	 * @param f A function from iteration number to an optional string.
	 * If the None, nothing will be printed.
	 * Otherwise, f(e) will be printed, where e is the current element being processed.
	 */
	def withCounter(f: Int => Option[String]) = new Iterator[T] {
		private var i = 0
		override def hasNext = {
			if ($.hasNext)
				true
			else {
				print("\r")
				false
			}

		}
		override def next() = {
			i += 1
			for (l <- f(i))
				print("\r" + l)
			$.next
		}
	}

	/**
	 * Returns an iterator that outputs to the console its progress in percentages
	 * @param size the total number of elements in the iterator
	 */
	def withPercentage(size: Int) = {
		var lastPercentage = 0
		withCounter(i => {
			val currentPercentage = i * 100 / size
			if (currentPercentage > lastPercentage) {
				lastPercentage = currentPercentage
				Some(s"$currentPercentage% done")
			} else None
		})
	}
	def zipWithIndex: Iterator[(T, Int)] = new Iterator[(T, Int)] {
		private var i = -1
		override def hasNext = $.hasNext
		override def next() = {
			i += 1
			($.next, i)
		}
	}

	def par(windowSize: Int = 20): Iterator[T] = new ParIterator($, windowSize)

	//TODO use implicits to avoid duplications
	def mapDefined[U](f: T => Option[U]): Iterator[U] = $ map f filter (_ isDefined) map (_ get)
}

object RichIterator {
	implicit def richIterator[T]($: Iterator[T]) = new RichIterator($)
}
