package common.rich.path

import java.io.{File, PrintStream}
import java.nio.file.FileAlreadyExistsException
import java.util.Scanner

import common.DirectorySpecs
import org.scalatest._

// TODO refactor to FreeSpec
class RichFileTest extends FlatSpec with Matchers with DirectorySpecs with OneInstancePerTest {
  private val f = tempFile

  def managed[T <: AutoCloseable](t: T)(f: T => Unit): Unit = {
    try f(t)
    finally t.close()
  }

  private def checkClosedAndDelete(f: File) {
    managed(new PrintStream(f)) {
      _.print("foobar2")
    }
    require(f.delete())
  }

  "Extension" should "has extension" in {
    val f = tempDir addFile "foo.bar"
    f.extension shouldBe "bar"
  }
  it should "has no extension" in {
    val f = tempDir addFile "foobar"
    f.extension shouldBe ""
  }

  private val $ = f
  "appendLine" should "append lines" in {
    $ appendLine "foobar"
    managed(new Scanner($)) { scanner =>
      scanner.nextLine shouldBe "foobar"
      scanner.hasNext shouldBe false
    }
  }
  it should "should append multiple lines" in {
    $ appendLine "foo"
    $ appendLine "bar"
    managed(new Scanner($)) { scanner =>
      scanner.nextLine shouldBe "foo"
      scanner.nextLine shouldBe "bar"
      scanner.hasNext shouldBe false
    }
  }

  "ReadAll" should "read single line" in {
    managed(new PrintStream($)) {
      _.println("foobar!")
    }

    $.readAll shouldBe "foobar!"
  }
  it should "read multiple lines" in {
    managed(new PrintStream($)) { ps =>
      ps.println("foobar!")
      ps.println("foobar2!")
    }
    $.readAll.matches("foobar!\r?\nfoobar2!") shouldBe true
  }
  it should "return an empty string when the file is empty" in {
    $.readAll shouldBe ""
  }
  it should "close" in {
    $.f.exists shouldBe true
    $.readAll
    checkClosedAndDelete($)
  }

  "lines" should "return the lines in the file" in {
    val list = List("foobar!", "barfoo?", "nope, definitely foobar")
    managed(new PrintStream($)) { ps => list.foreach(ps.println) }
    $.lines.toList shouldBe list
  }

  it should "foobar" in {
    f.appendLine("foobar")
    f.lines
    checkClosedAndDelete(f)
  }

  private val bytes1 = Array[Byte](1, 2, 3)
  private val bytes2 = Array[Byte](4, 5, 6)
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

  it should "close" in {
    val f1 = tempDir.addFile()
    val f2 = tempDir.addFile()
    f1.write(bytes1)
    f2.write(bytes2)
    f1.hasSameContentAs(f2)
    checkClosedAndDelete(f1)
    checkClosedAndDelete(f2)
  }

  "copyTo" should "copy the file" in {
    val f1 = tempDir.addFile().write("foo")
    val newFile = f1.copyTo(tempDir, "new_file")
    newFile.name shouldBe "new_file"
    newFile.readAll shouldBe "foo"
    checkClosedAndDelete(f1)
    checkClosedAndDelete(newFile)
  }

  it should "throw an exception if the file already exists" in {
    val f1 = tempDir.addFile().write("foo")
    val f2 = tempDir.addFile("bar.txt").write("bar")
    an[FileAlreadyExistsException] should be thrownBy f1.copyTo(tempDir, "bar.txt")
    f2.readAll shouldBe "bar"
    checkClosedAndDelete(f1)
    checkClosedAndDelete(f2)
  }

  "path" should "be canonical" in {
    val f1 = tempDir addFile "f1"
    val f2 = tempDir / "./f1"
    f1.path should be === f2.path
  }
}
