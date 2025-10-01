package common.rich.primitives

import java.io.{ByteArrayInputStream, File, InputStream, PrintStream}
import java.util.StringTokenizer
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.util.matching.Regex

import common.rich.RichT._
import common.rich.RichTuple.richTuple2
import common.rich.path.RichFile.richFile
import common.rich.primitives.RichBoolean._

object RichString {
  implicit class richString(private val $ : String) extends AnyVal {
    def unquote: String = replaceAll(WrappingQuotes, "")
    def quote: String = '"' + $ + '"'
    def isWhitespaceOrEmpty: Boolean = $.trim.isEmpty
    def appendTo(f: File): Unit = f.appendLine($)

    private def parse(splitBy: String, expectedSize: Int): Array[String] = {
      val split = $.split(splitBy)
      require(split.length == expectedSize)
      split
    }
    type Parser[A] = String => A
    def splitParse[A, B](splitBy: String, fa: Parser[A], fb: Parser[B]): (A, B) = {
      val split = parse(splitBy, 2)
      (fa(split(0)), fb(split(1)))
    }
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

    /** Does not return a sequence of delimiters at the end. */
    def smartSplit(regex: String): Seq[String] = $.split(regex)
      .mapIf($.endsWith(regex).const)
      .to(_ :+ "") // end in "" if ends with regex
    /** Does not return a sequence of delimiters at the end. */
    def smartSplit(c: Char): Seq[String] = smartSplit(c.toString)
    /**
     * Adds the delimiters to the returned sequence. The split regex will be returns as a single
     * element in the returned sequence. For example:
     * {{{
     *   "foo ,;. bar".splitWithDelimiters("[, ;. ]+") == Seq("foo", " ,;. ", "bar")
     * }}}
     */
    def splitWithDelimiters(pattern: String): Seq[String] = splitWithDelimiters(
      Pattern.compile(pattern),
    )
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

    def toInputStream: InputStream = new ByteArrayInputStream($.getBytes)

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
