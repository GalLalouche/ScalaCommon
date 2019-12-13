package common.rich.primitives

import java.util.regex.Pattern

import org.scalatest.FreeSpec

import common.AuxSpecs
import common.rich.primitives.RichString._

class RichStringTest extends FreeSpec with AuxSpecs {
  "unquote" - {
    "does nothing when no quotes" in {
      "foobar".unquote shouldReturn "foobar"
    }
    "removes a single pair of quotes" in {
      "\"foobar\"".unquote shouldReturn "foobar"
    }
    "removes more than a single, unmatching quotes" in {
      "\"foobar\"\"".unquote shouldReturn "foobar"
    }
  }
  "quote" in {
    "foobar".quote shouldReturn "\"foobar\""
  }

  "isWhitespaceOrEmpty" in {
    " \t\t \n\t ".isWhitespaceOrEmpty shouldReturn true
    "foo bar".isWhitespaceOrEmpty shouldReturn false
  }

  "smartSplit" - {
    "returns an empty string if the last item is the delimiter" in {
      "a,".smartSplit(',') shouldReturn Seq("a", "")
    }
    "returns an empty string if the last item is the delimiters" in {
      "a,,,".smartSplit(',') shouldReturn Seq("a", "")
    }
  }

  "splitWithDelimiters" - {
    "returns the actual string when there are no delimiters" in {
      "this is a normal string".splitWithDelimiters(",") shouldReturn Vector("this is a normal string")
    }
    "returns the string with delimiters" in {
      "this.is.a.test".splitWithDelimiters("\\.") shouldReturn
          Vector("this", ".", "is", ".", "a", ".", "test")
      ",a,b,,".splitWithDelimiters(",") shouldReturn ",a,b,,".toVector.map(_.toString)
    }
    "returns delimiters as a single string" in {
      "this.,is.:a.;test".splitWithDelimiters("[.,:;]+") shouldReturn
          Vector("this", ".,", "is", ".:", "a", ".;", "test")
    }
  }

  "captureWith" - {
    "captures the first argument" in {
      "12345hello5769820".captureWith("""\d+([a-z]+)\d+""".r) shouldReturn "hello"
    }
    "captures .*" in {
      "\"D:\\Program Files (x86)\\Evince-2.32.0.145\\bin\\evince.exe\" \"%1\""
          .captureWith(""""([^"]+)".*""".r) shouldReturn "D:\\Program Files (x86)\\Evince-2.32.0.145\\bin\\evince.exe"
    }
  }

  "fromPrintStream" in {
    RichString.fromPrintStream(_.write("foobar".getBytes)) shouldReturn "foobar"
  }

  "matches" - {
    "true" in {
      "foobar".matches(Pattern.compile("f.{4}r")) shouldReturn true
    }
    "false" in {
      "barfoo".matches(Pattern.compile("f.{4}r")) shouldReturn false
    }
  }

  "replaceAll" in {
    "foobar".replaceAll(Pattern.compile(".b"), "xx") shouldReturn "foxxar"
  }

  "simpleReplace" - {
    "no match" in {
      "foobarbazz".simpleReplace("", "xyy") shouldReturn "foobarbazz"
    }
    "actual" in {
      "foobarbazz".simpleReplace("ba", "xyy") shouldReturn "fooxyyrxyyzz"
    }
  }
  "simpleRemove" in {
    "foobarbazz".simpleRemove("ba") shouldReturn "foorzz"
  }

  "removeAll" in {
    "foobar".removeAll(Pattern.compile(".b")) shouldReturn "foar"
  }

  "dropAfterLast" - {
    "no character returns same string" in {
      "foobar" dropAfterLast 'q' shouldReturn "foobar"
    }
    "last character returns same string" in {
      "foobar" dropAfterLast 'r' shouldReturn "foobar"
    }
    "first character" in {
      "foobar" dropAfterLast 'f' shouldReturn "f"
    }
    "middle" in {
      "foobar" dropAfterLast 'o' shouldReturn "foo"
    }
  }

  "takeAfterLast" - {
    "no character returns same string" in {
      "foobar" takeAfterLast 'q' shouldReturn "foobar"
    }
    "last character returns empty" in {
      "foobar" takeAfterLast 'r' shouldReturn ""
    }
    "first character" in {
      "foobar" takeAfterLast 'f' shouldReturn "oobar"
    }
    "middle" in {
      "foobar" takeAfterLast 'o' shouldReturn "bar"
    }
  }
}
