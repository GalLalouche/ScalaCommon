package common.rich.path

import common.DirectorySpecs
import org.scalatest._

class BackupFileTest extends FlatSpec with Matchers with DirectorySpecs with OneInstancePerTest {
  val f = tempFile.appendLine("Foobar")
  val backup = f.backup
  "restore" should "override any changes" in {
    f.clear.appendLine("Barfoo")
    f.readAll shouldBe "Barfoo"
    backup.restore()
    f.readAll shouldBe "Foobar"
  }

  it should "work with cleared files" in {
    f.clear
    backup.restore()
    f.readAll shouldBe "Foobar"
  }

  it should "work with deleted files" in {
    f.delete
    backup.restore()
    f.readAll shouldBe "Foobar"
  }
}