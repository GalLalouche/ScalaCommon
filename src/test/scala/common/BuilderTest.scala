package common

import org.scalatest.FreeSpec

import common.test.AuxSpecs

class BuilderTest extends FreeSpec with AuxSpecs {
  class TestBuilder(map: Map[String, Any]) extends Builder[TestBuilder](map) {
    def this() = this(Map("foo" -> "bar"))
    override protected def clone(parameters: Map[String, Any]): TestBuilder = new TestBuilder(parameters)
    def foo(s: String) = update(s)
    def getFoo: String = map("foo").asInstanceOf[String]
    //meh, asInstanceOf isn't safe anyway
    override def asT[T](key: String): T = super.asT(key)
  }

  "update should figure out the parameter type" in {
    new TestBuilder().foo("baz").getFoo shouldReturn "baz"
  }

  "asT should return the correctType" in {
    def aux(s: String) = s
    aux(new TestBuilder().asT("foo")) shouldReturn "bar"
  }
}
