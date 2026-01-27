package common.rich.path

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec

import common.rich.path.RichFile.richFile
import common.test.{AuxSpecs, DirectorySpecs}

class BackupFileTest extends AnyFreeSpec with AuxSpecs with DirectorySpecs with OneInstancePerTest {
  private val f = tempFile.appendLine("Foobar")
  private val backup = f.backup
  "restore" - {
    "overrides any changes" in {
      f.clear().appendLine("Barfoo")
      f.readAll shouldBe "Barfoo"
      backup.restore()
      f.readAll shouldBe "Foobar"
    }

    "works with cleared files" in {
      f.clear()
      backup.restore()
      f.readAll shouldBe "Foobar"
    }

    "works with deleted files" in {
      f.delete
      backup.restore()
      f.readAll shouldBe "Foobar"
    }
  }
}
