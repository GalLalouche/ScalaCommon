package common.rich.path

import java.io.{File, PrintStream}
import java.nio.file.FileAlreadyExistsException
import java.util.Scanner

import common.{AuxSpecs, DirectorySpecs}
import org.scalatest._

class RichFileTest extends FreeSpec with AuxSpecs with DirectorySpecs with OneInstancePerTest {
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

  "Extension" - {
    "has extension" in {
      tempDir.addFile("foo.bar").extension shouldBe "bar"
    }
    "has no extension should return an empty string" in {
      tempDir.addFile("foobar").extension shouldBe ""
    }
  }

  "appendLine" - {
    "single line" in {
      f appendLine "foobar"
      managed(new Scanner(f)) { scanner =>
        scanner.nextLine shouldBe "foobar"
        scanner.hasNext shouldBe false
      }
    }
    "multiple lines" in {
      f appendLine "foo"
      f appendLine "bar"
      managed(new Scanner(f)) { scanner =>
        scanner.nextLine shouldBe "foo"
        scanner.nextLine shouldBe "bar"
        scanner.hasNext shouldBe false
      }
    }
  }

  "readAll" - {
    "single line" in {
      managed(new PrintStream(f)) {
        _.println("foobar!")
      }

      f.readAll shouldBe "foobar!"
    }
    "multiple lines" in {
      managed(new PrintStream(f)) { ps =>
        ps.println("foobar!")
        ps.println("foobar2!")
      }
      f.readAll.matches("foobar!\r?\nfoobar2!") shouldBe true
    }
    "returns an empty string when the file is empty" in {
      f.readAll shouldBe ""
    }
    "closes" in {
      f.f.exists shouldBe true
      f.readAll
      checkClosedAndDelete(f)
    }
  }

  "lines" - {
    "returns the lines in the file" in {
      val list = List("foobar!", "barfoo?", "nope, definitely foobar")
      managed(new PrintStream(f)) { ps => list.foreach(ps.println) }
      f.lines.toList shouldBe list
    }

    "closes" in {
      f.appendLine("foobar")
      f.lines
      checkClosedAndDelete(f)
    }
  }

  "hasSameContents" - {
    val bytes1 = Array[Byte](1, 2, 3)
    val bytes2 = Array[Byte](4, 5, 6)
    "true for equals" in {
      val f1 = tempDir.addFile()
      val f2 = tempDir.addFile()
      f1.write(bytes1)
      f2.write(bytes1)
      f1.hasSameContentAs(f2)
    }
    "false for different" in {
      val f1 = tempDir.addFile()
      val f2 = tempDir.addFile()
      f1.write(bytes1)
      f2.write(bytes2)
      f1.hasSameContentAs(f2) shouldBe false
    }

    "closes" in {
      val f1 = tempDir.addFile()
      val f2 = tempDir.addFile()
      f1.write(bytes1)
      f2.write(bytes2)
      f1.hasSameContentAs(f2)
      checkClosedAndDelete(f1)
      checkClosedAndDelete(f2)
    }
  }

  "copyTo" - {
    "copies the file" in {
      val f1 = tempDir.addFile().write("foo")
      val newFile = f1.copyTo(tempDir, "new_file")
      newFile.name shouldBe "new_file"
      newFile.readAll shouldBe "foo"
      checkClosedAndDelete(f1)
      checkClosedAndDelete(newFile)
    }

    "throws an exception if the file already exists" in {
      val f1 = tempDir.addFile().write("foo")
      val f2 = tempDir.addFile("bar.txt").write("bar")
      an[FileAlreadyExistsException] should be thrownBy f1.copyTo(tempDir, "bar.txt")
      f2.readAll shouldBe "bar"
      checkClosedAndDelete(f1)
      checkClosedAndDelete(f2)
    }
  }

  "path is canonical" in {
    val f1 = tempDir addFile "f1"
    val f2 = tempDir / "./f1"
    f1.path shouldReturn f2.path
  }
}
