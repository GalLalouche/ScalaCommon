package common.rich.primitives

import common.AuxSpecs
import common.rich.primitives.RichString._
import org.scalatest.FlatSpec

class RichStringTest extends FlatSpec with AuxSpecs {

	"removeTrailingQuotes" should "do nothing when no quotes" in {
		"foobar".withoutTrailingQuotes shouldReturn "foobar"
	}

	it should "remove a single pair of quotes" in {
		"\"foobar\"".withoutTrailingQuotes shouldReturn "foobar"
	}

	it should "remove more than a single, unmatching quotes" in {
		"\"foobar\"\"".withoutTrailingQuotes shouldReturn "foobar"
	}

	"smartSplit" should "return an empty string if the last item is the delimiter" in {
		"a,".smartSplit(',') shouldReturn Seq("a", "")
	}

	it should "return an empty string if the last item is the delimiters" in {
		"a,,,".smartSplit(',') shouldReturn Seq("a", "")
	}

	"splitWithDelimiters" should "return the actual string when there are no delimiters" in {
		"this is a normal string".splitWithDelimiters(",") shouldReturn List("this is a normal string")
	}

	it should "return the string with delimiters" in {
		"this.is.a.test".splitWithDelimiters("\\.") shouldReturn
        List("this", ".", "is", ".", "a", ".", "test")
		",a,b,,".splitWithDelimiters(",") shouldReturn ",a,b,,".toList.map(_.toString)
	}

	"captureWith" should "capture the first argument" in {
		"12345hello5769820".captureWith( """\d+([a-z]+)\d+""".r) shouldReturn "hello"
	}

	it should "capture .*" in {
		"\"D:\\Program Files (x86)\\Evince-2.32.0.145\\bin\\evince.exe\" \"%1\""
			.captureWith( """"([^"]+)".*""".r) shouldReturn "D:\\Program Files (x86)\\Evince-2.32.0.145\\bin\\evince.exe"
	}
}
