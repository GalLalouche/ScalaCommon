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

    type Parser[A] = String => A
    def splitParse[A, B](splitBy: String, fa: Parser[A], fb: Parser[B]): (A, B) = {
      val split = $.split(splitBy)
      require(split.size == 2)
      (fa(split(0)), fb(split(1)))
    }
    def splitParse[A, B, C](splitBy: String, fa: Parser[A], fb: Parser[B], fc: Parser[C]): (A, B, C) = {
      val split = $.split(splitBy)
      require(split.size == 3)
      (fa(split(0)), fb(split(1)), fc(split(2)))
    }
    def splitParse[A, B, C, D](splitBy: String,
        fa: Parser[A], fb: Parser[B], fc: Parser[C], fd: Parser[D]): (A, B, C, D) = {
      val split = $.split(splitBy)
      require(split.size == 4)
      (fa(split(0)), fb(split(1)), fc(split(2)), fd(split(3)))
    }
    def splitParse[A, B, C, D, E](splitBy: String,
        fa: Parser[A], fb: Parser[B], fc: Parser[C], fd: Parser[D], fe: Parser[E]): (A, B, C, D, E) = {
      val split = $.split(splitBy)
      require(split.size == 5)
      (fa(split(0)), fb(split(1)), fc(split(2)), fd(split(3)), fe(split(4)))
    }
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

    /** If c isn't present in the string, returns the same string. */
    def dropAfterLast(c: Char): String = {
      val dropFrom: Int = $ lastIndexOf c
      if (dropFrom == -1) $ else $.substring(0, dropFrom + 1)
    }
    /** If c isn't present in the string, returns the same string. */
    def takeAfterLast(c: Char): String = $.substring($.lastIndexOf(c) + 1)
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
