package common.rich.path

import java.io.{File, PrintStream}
import java.util.Scanner

import common.DirectorySpecs
import org.scalatest._

class RichFileTest extends FlatSpec with Matchers with DirectorySpecs with OneInstancePerTest {
  val f = tempFile

  import resource._

  private def checkClosed(f: File) {
    managed(new PrintStream(f)).acquireAndGet {
      _.print("foobar2")
    }
    require(f.delete())
  }

  "Extension" should "has extension" in {
    val f = tempDir.addFile("foo.bar")
    f.extension shouldBe "bar"
  }
  it should "has no extension" in {
    val f = tempDir.addFile("foobar")
    f.extension shouldBe ""
  }

  val $ = f
  "Write" should "write string" in {
    $.appendLine("foobar")
    managed(new Scanner($)).acquireAndGet {
      scanner =>
        scanner.nextLine shouldBe "foobar"
        scanner.hasNext shouldBe false
    }
  }
  it should "should foobar" in {
    $.appendLine("foo")
    $.appendLine("bar")
    managed(new Scanner($)).acquireAndGet {
      scanner =>
        scanner.nextLine shouldBe "foo"
        scanner.nextLine shouldBe "bar"
        scanner.hasNext shouldBe false
    }
  }

  "ReadAll" should "read single line" in {
    for (ps <- managed(new PrintStream($)))
      ps.println("foobar!")
    $.readAll shouldBe "foobar!"
  }
  it should "read multiple lines" in {
    for (ps <- managed(new PrintStream($))) {
      ps.println("foobar!")
      ps.println("foobar2!")
    }
    $.readAll.matches("foobar!\r?\nfoobar2!") shouldBe true
  }
  it should "return an empty string when the file is empty" in {
    $.readAll shouldBe ""
  }
  it should "foobar" in {
    $.f.exists shouldBe true
    $.readAll
    checkClosed($)
  }

  "lines" should "return the lines in the file" in {
    val list = List("foobar!", "barfoo?", "nope, definitely foobar")
    for (ps <- managed(new PrintStream($)))
      list.foreach(ps.println)
    $.lines.toList shouldBe list
  }

  it should "foobar" in {
    f.appendLine("foobar")
    f.lines
    checkClosed(f)
  }
  val bytes1 = Array[Byte](1, 2, 3)
  val bytes2 = Array[Byte](4, 5, 6)
  "hasSameContent" should "true for equals" in {
    val f1 = tempDir.addFile()
    val f2 = tempDir.addFile()
    f1.write(bytes1)
    f2.write(bytes1)
    f1.hasSameContentAs(f2)
  }
  it should "false for different" in {
    val f1 = tempDir.addFile()
    val f2 = tempDir.addFile()
    f1.write(bytes1)
    f2.write(bytes2)
    f1.hasSameContentAs(f2) shouldBe false
  }

  it should "foobar" in {
    val f1 = tempDir.addFile()
    val f2 = tempDir.addFile()
    f1.write(bytes1)
    f2.write(bytes2)
    f1.hasSameContentAs(f2)
    checkClosed(f1)
    checkClosed(f2)
  }

  it should "foobar both" in {
    val f1 = tempDir.addFile()
    val f2 = tempDir.addFile()
    f1.write("Hello world!")
    f1.copyTo(f2)
    checkClosed(f1)
    checkClosed(f2)
  }

  "path" should "be canonical" in {
    val f1 = tempDir addFile ("f1")
    val f2 = tempDir / "./f1"
    f1.path should be === f2.path
  }
}
