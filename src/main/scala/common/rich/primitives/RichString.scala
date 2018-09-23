package common.rich.primitives

import java.io.{ByteArrayInputStream, File, InputStream, PrintStream}
import java.util.regex.Pattern

import common.rich.primitives.RichBoolean._
import common.rich.RichT.{richT, _}
import common.rich.path.RichFile.richFile

import scala.util.matching.Regex

object RichString {
  implicit class richString(private val $: String) extends AnyVal {
    def withoutTrailingQuotes: String = $.replaceAll("""^["']+|["']+$""", "")
    def isWhitespaceOrEmpty: Boolean = $ matches "\\s*"
    def appendTo(f: File): Unit = f appendLine $

    /** does not return a sequence of delimiters at the end */
    def smartSplit(regex: String): Seq[String] = $
        .split(regex)
        .mapIf($.endsWith(regex).const).to(_ :+ "") // end in "" if ends with regex

    /** splits last item too */
    def smartSplit(c: Char): Seq[String] = smartSplit(c.toString)

    /** adds the delimiters to the returned sequence */
    def splitWithDelimiters(pattern: String): Seq[String] =
      $.foldLeft((List[String](), new StringBuilder)) {
        case ((agg, sb), c) =>
          if (c.toString.matches(pattern)) (c.toString :: sb.toString :: agg, new StringBuilder) // delimiter
          else (agg, sb append c)
      }.mapTo(e => e._2.toString :: e._1) // append last SB to list
          .filterNot(_.isEmpty) // remove empty ""
          .reverse

    def captureWith(regex: Regex): String = $ match {
      case regex(result) => result
    }

    def dropAfterLast(c: Char): String = $.substring($.lastIndexOf(c) + 1)
    def toInputStream: InputStream = new ByteArrayInputStream($.getBytes)

    def matches(p: Pattern): Boolean = p.matcher($).matches()
    def doesNotMatch(p: Pattern): Boolean = matches(p).isFalse
  }

  /** Reads what's written to the PrintStream and writes it to the output string. */
  def fromPrintStream(f: PrintStream => Any): String = {
    import java.io.{ByteArrayOutputStream, PrintStream}
    import java.nio.charset.StandardCharsets

    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos, true, "utf-8")
    try f(ps)
    finally ps.close()
    new String(baos.toByteArray, StandardCharsets.UTF_8)
  }
}
