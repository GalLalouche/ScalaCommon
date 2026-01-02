package common.rich

import java.io.ByteArrayOutputStream

import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.freespec.AnyFreeSpec

import common.rich.RichInputStream._
import common.rich.primitives.RichString._
import common.test.AuxSpecs

class RichInputStreamTest extends AnyFreeSpec with AuxSpecs {
  "readLine" in {
    val is = "foo\r\nבאר\n\nbaz".toInputStream
    is.readLine().value shouldReturn "foo"
    is.readLine().value shouldReturn "באר"
    is.readLine().value shouldReturn ""
    is.readLine().value shouldReturn "baz"
    is.readLine() shouldBe empty
  }
  "asString" in "foobar".toInputStream.asString().shouldReturn("foobar")
  "toBytes" in "foobar".toInputStream.toBytes().shouldReturn("foobar".getBytes)
  "write to OutputStream" in {
    val baos = new ByteArrayOutputStream()
    "foobar".toInputStream.writeTo(baos)
    new String(baos.toByteArray, "UTF-8") shouldReturn "foobar"
  }
}
