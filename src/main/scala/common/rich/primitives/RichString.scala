package common.rich.primitives

import java.io.{ByteArrayInputStream, File, InputStream, PrintStream}
import java.util.StringTokenizer
import java.util.regex.Pattern

import com.google.common.base.Charsets

import scala.annotation.tailrec
import scala.util.matching.Regex

import common.rich.RichFile.richFile
import common.rich.RichT._
import common.rich.RichTuple.richTuple2
import common.rich.primitives.RichBoolean._

object RichString {
  implicit class richString(private val $ : String) extends AnyVal {
    def unquote: String = replaceAll(WrappingQuotes, "")
    def quote: String = (new StringBuilder).append('"').append($).append('"').toString()
    def isWhitespaceOrEmpty: Boolean = $.forall(_.isWhitespace)
    def appendTo(f: File): Unit = f.appendLine($)

    type Parser[A] = String => A
    def splitParse[A, B](splitBy: String, fa: Parser[A], fb: Parser[B]): (A, B) = {
      val split = parse(splitBy, 2)
      (fa(split(0)), fb(split(1)))
    }
    def osplitParse[A, B](splitBy: String, fa: Parser[A], fb: Parser[B]): Option[(A, B)] =
      oparse(splitBy, 2).map(s => (fa(s(0)), fb(s(1))))
    def splitParse[A, B, C](
        splitBy: String,
        fa: Parser[A],
        fb: Parser[B],
        fc: Parser[C],
    ): (A, B, C) = {
      val split = parse(splitBy, 3)
      (fa(split(0)), fb(split(1)), fc(split(2)))
    }
    def splitParse[A, B, C, D](
        splitBy: String,
        fa: Parser[A],
        fb: Parser[B],
        fc: Parser[C],
        fd: Parser[D],
    ): (A, B, C, D) = {
      val split = parse(splitBy, 4)
      (fa(split(0)), fb(split(1)), fc(split(2)), fd(split(3)))
    }
    def splitParse[A, B, C, D, E](
        splitBy: String,
        fa: Parser[A],
        fb: Parser[B],
        fc: Parser[C],
        fd: Parser[D],
        fe: Parser[E],
    ): (A, B, C, D, E) = {
      val split = parse(splitBy, 5)
      (fa(split(0)), fb(split(1)), fc(split(2)), fd(split(3)), fe(split(4)))
    }
    private def parse(splitBy: String, expectedSize: Int): Array[String] =
      oparse(splitBy, expectedSize).getOrElse {
        val msg =
          s"Expected $expectedSize elements when splitting by '$splitBy', but got ${$.split(splitBy).length}"
        throw new IllegalArgumentException(msg)
      }
    private def oparse(splitBy: String, expectedSize: Int): Option[Array[String]] =
      $.split(splitBy).optFilter(_.length == expectedSize)

    /** Does not return a sequence of delimiters at the end. */
    /** Does not return a sequence of delimiters at the end. */
    def smartSplit(c: Char): Seq[String] = $.split(c)
      .mapIf($.nonEmpty && $.last == c)
      .to(_ :+ "") // end in "" if ends with c
    /**
     * Adds the delimiters to the returned sequence. The split regex will be returned as a single
     * element in the returned sequence. For example:
     * {{{
     *   "foo ,;. bar".splitWithDelimiters("[, ;. ]+") == Seq("foo", " ,;. ", "bar")
     * }}}
     */
    def splitWithDelimiters(pattern: Pattern): Seq[String] = {
      @tailrec
      def go(input: String, result: List[String]): List[String] = {
        val m = pattern.matcher(input)
        if (m.find().isFalse)
          return input :: result
        val start = m.start(0)
        val end = m.end(0)
        val head = input.take(start)
        val delim = input.substring(start, end)
        go(input.substring(end), delim :: head :: result)
      }
      go($, Nil).reverseIterator.filterNot(_.isEmpty).toVector
    }

    def endsWithCaseInsensitive(suffix: String): Boolean =
      $.regionMatches(true /* ignoreCase */, $.length - suffix.length, suffix, 0, suffix.length)

    def captureWith(regex: Regex): String = $ match { case regex(result) => result }

    /** If c isn't present in the string, returns the same string. */
    def dropAfterLast(c: Char): String = {
      val dropFrom = $.lastIndexOf(c)
      if (dropFrom == -1) $ else $.substring(0, dropFrom + 1)
    }
    /** If c isn't present in the string, returns the same string. */
    def takeAfterLast(c: Char): String = $.substring($.lastIndexOf(c) + 1)

    def matches(p: Pattern): Boolean = p.matcher($).matches()
    def doesNotMatch(p: Pattern): Boolean = matches(p).isFalse
    def containsMatch(p: Pattern): Boolean = p.matcher($).find()
    def doesNotContainMatch(p: Pattern): Boolean = containsMatch(p).isFalse

    def replaceAll(p: Pattern, replacement: String): String = p.matcher($).replaceAll(replacement)

    def removeAll(p: Pattern): String = replaceAll(p, "")
    def removeAll(regex: String): String = removeAll(Pattern.compile(regex))

    def toInputStream: InputStream = new ByteArrayInputStream($.getBytes(Charsets.UTF_8))

    /** Performs a literal string replace without compiling a regular expression. */
    def simpleReplace(search: String, replace: String): String =
      StringUtils.replace($, search, replace)
    /** Performs a literal string removal without compiling a regular expression. */
    def simpleRemove(search: String): String = simpleReplace(search, "")

    def split(p: Pattern): Array[String] = p.split($)

    def longestCommonSuffix(other: String): String =
      $.takeRight($.reverseIterator.zip(other.reverseIterator).takeWhile(_.reduce(_ == _)).length)

    /** `tokens` should be in the same format as those passed to [[StringTokenizer]]. */
    def tokenize(tokens: String): Iterator[String] = {
      val st = new StringTokenizer($, tokens)
      // For whatever backassward reason, StringTokenizer implements Enumeration<Object>, not
      // Enumeration<String> :/
      new Iterator[String] {
        override def hasNext = st.hasMoreTokens
        override def next() = st.nextToken()
      }
    }
  }

  private val WrappingQuotes = Pattern.compile("""^["']+|["']+$""")
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
