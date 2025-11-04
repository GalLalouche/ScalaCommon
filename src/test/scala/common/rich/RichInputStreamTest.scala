package common.rich

import java.io.ByteArrayOutputStream

import org.scalatest.freespec.AnyFreeSpec

import common.rich.RichInputStream._
import common.rich.primitives.RichString._
import common.test.AuxSpecs

class RichInputStreamTest extends AnyFreeSpec with AuxSpecs {
  "asString" in "foobar".toInputStream.asString.shouldReturn("foobar")
  "toBytes" in "foobar".toInputStream.toBytes.shouldReturn("foobar".getBytes)
  "write to OutputStream" in {
    val baos = new ByteArrayOutputStream()
    "foobar".toInputStream.writeTo(baos)
    new String(baos.toByteArray, "UTF-8") shouldReturn "foobar"
  }
}
