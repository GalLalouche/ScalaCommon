package common

import java.util.Date

import common.rich.RichAll._
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.joda.time.{DateTime, DateTimeZone}

/**
 * Tries several parsers in a sequence until the first one succeeds
 */
class CompositeDateFormat private(formatters: Seq[DateTimeFormatter]) {
	require(formatters.nonEmpty)

	def parse(source: String): DateTime = {
		try
			formatters
				.iterator
				.tryMapSilent(_.parseDateTime(source))
				.next
		catch {
			case e: NoSuchElementException => throw new Exception("No formatter could parse " + source)
		}
	}

	def print(date: Date): String = print(date.getTime)
	def print(date: DateTime): String = print(date.getMillis)
	def print(time: Long): String = formatters.head.print(time)
}

object CompositeDateFormat {
	def apply(patterns: String*) = new CompositeDateFormat(patterns
		.map(DateTimeFormat.forPattern)
		.map(_.withZone(DateTimeZone.UTC))
	)
}
