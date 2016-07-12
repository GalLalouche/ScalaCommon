package common.rich

import common.AuxSpecs
import common.rich.RichT._
import org.scalatest.FreeSpec

class RichTTest extends FreeSpec with AuxSpecs {
	"opt" - {
		"None" in {
			val x: Any = null
			x.opt shouldReturn None
		}
		"Some" in {5.opt shouldReturn Some(5)}
	}
	"only if" - {
		"int" in {5.onlyIf(false) shouldReturn 0}
		"double" in {5.0.onlyIf(false) shouldReturn 0.0}
		"string" in {"foobar".onlyIf(false) shouldReturn ""}
	}
	"safe cast" - {
		"correct" in {
			val x: Any = "Foobar"
			x.safeCast[String].get shouldReturn "Foobar"
		}
		"None" in {
			val x: Any = 17
			x.safeCast[String] shouldReturn None
		}
	}
}
