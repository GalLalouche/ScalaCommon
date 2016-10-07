package common

import org.scalatest.{FreeSpec, OneInstancePerTest}

class LazyMapTests extends FreeSpec with AuxSpecs with OneInstancePerTest {
	"ctor" - {
		"should fail on null" in {
			an[IllegalArgumentException] should be thrownBy {LazyMap(null)}
		}
	}

	"applications" - {
		val f = new Function[Int, Int] {
			var count = 0;
			override def apply(v1: Int): Int = {
				count += 1
				v1 * 2
			}

		}
		val $ = LazyMap(f)
		"new values" - {
			"apply should return the value always" in {$.apply(12) shouldReturn 24}
			"get should return an optional" in {$.get(12) shouldReturn None}
			"update should return a new instance which may or may not be this" in {$.update(12)._1 shouldReturn 24}
		}
		"existing values" - {
			val $ = LazyMap(f).update(12)._2
			"apply" - {
				"should return the value if it already exists" in {$.apply(12) shouldReturn 24}
				"f should not be called for a value that already exists" in {$.apply(12); f.count shouldReturn 1}
			}
			"get" - {
				"should return the value if it already exists" in {$.get(12) shouldReturn Option(24)}
				"f should not be called for a value that already exists" in {$.get(12); f.count shouldReturn 1}
				"should not return a value if it doesn't exist" in {$.get(13) shouldReturn None}
				"f should not be applied if the value doesn't exist" in {$.get(13); f.count shouldReturn 1}
			}
			"update" - {
				"should return the value if it already exists" in {$.update(12)._1 shouldReturn 24}
				"f should not be called for a value that already exists" in {$.update(12); f.count shouldReturn 1}
				"update should return this if the value already exists" in {$.update(12)._2 shouldReturn $}
			}
		}
	}
}
