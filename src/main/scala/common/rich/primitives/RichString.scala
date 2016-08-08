package common.rich.primitives

import java.io.{ByteArrayInputStream, File}

import common.rich.RichT.richT
import common.rich.path.RichFile.richFile

import scala.util.matching.Regex

object RichString {
  implicit class richString($: String) {
    def withoutTrailingQuotes = $.replaceAll("""^["']+|["']+$""", "")
    def isWhitespaceOrEmpty = $ matches "\\s*"
    def appendTo(f: File) = f appendLine $

    /** does not return a sequence of delimiters at the end */
    def smartSplit(regex: String) = $
      .split(regex)
      .mapIf(e => $ endsWith regex).to(_ :+ "") // end in "" if ends with regex

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

    def dropAfterLast(c: Char) = $.substring($.lastIndexOf(c) + 1)
    def toInputStream = new ByteArrayInputStream($.getBytes)
    def capitalize: String = $.head.toUpper + $.tail.toLowerCase
  }
}
