package common.rich.path

import java.nio.file.FileAlreadyExistsException

import better.files.{File => BFile, FileExtensions}
import org.scalatest.OneInstancePerTest
import org.scalatest.freespec.AnyFreeSpec

import common.rich.RichT.richT
import common.rich.path.RichFile.richFile
import common.rich.path.ref.io.{IODirectory, TempDirectory}
import common.test.DirectorySpecs

class RichFileUtilsTest extends AnyFreeSpec with DirectorySpecs with OneInstancePerTest {
  private lazy val dir2 = TempDirectory()
  "move file" - {
    tempFile.write("foobar")
    val tempFileName = tempFile.getName
    val otherFile = tempDir.addFile("other_file")
    otherFile.write("bazz")
    def verifyFile(f: java.io.File, name: String = tempFileName): Unit = {
      f.exists shouldReturn true
      f.readAll shouldReturn "foobar"
      f.getName shouldReturn name
    }
    def verifyNoChange(): Unit = {
      verifyFile(tempFile, tempFileName)
      otherFile.exists shouldReturn true
      otherFile.name shouldReturn "other_file"
      otherFile.readAll shouldReturn "bazz"
    }
    "default to same name" in {
      val newFile = RichFileUtils.move(tempFile, dir2)
      tempFile.exists shouldReturn false
      verifyFile(newFile)
    }
    "happy path" in {
      val newFile = RichFileUtils.move(tempFile, dir2, "new_name.txt")
      tempFile.exists shouldReturn false
      newFile.parent shouldReturn dir2
      verifyFile(newFile, "new_name.txt")
    }
    "Existing file with same name throws" in {
      dir2.addFile("new_name.txt")
      a[FileAlreadyExistsException] should be thrownBy RichFileUtils.move(
        tempFile,
        dir2,
        "new_name.txt",
      )
      verifyNoChange()
    }
    "within same directory" - {
      "happy path" in {
        val newFile = RichFileUtils.rename(tempFile, tempFileName + "foo")
        tempFile.exists shouldReturn false
        verifyFile(newFile, tempFileName + "foo")
      }
      "same name" in {
        RichFileUtils.rename(tempFile, tempFileName)
        verifyNoChange()
      }
      "file with name already exists" in {
        a[FileAlreadyExistsException] should be thrownBy RichFileUtils.rename(
          tempFile,
          otherFile.name,
        )
        verifyNoChange()
      }
    }
  }
  "directory movers" - {
    def assertEmptyDir(d: IODirectory) = {
      d.dirs shouldBe empty
      d.files shouldBe empty
    }
    val targetDir = dir2
    val originalCopy =
      better.files
        .File(filledDir.toPath)
        .copyTo(BFile(filledDir.parent.toScala, filledDir.name + "_clone"))
        .|>(IODirectory apply _.toJava)
    val originalName = filledDir.name
    "move directory" - {
      "happy path" in {
        val movedDir = RichFileUtils.move(filledDir, targetDir)
        movedDir.parent shouldReturn targetDir
        assert(movedDir.name == originalName)
        filledDir.exists shouldReturn false
        assertSameContents(originalCopy, movedDir)
      }
      "Existing directory with same name throws" in {
        targetDir.addSubDir(filledDir.name)
        an[FileAlreadyExistsException] should be thrownBy RichFileUtils.move(filledDir, targetDir)
        assertSameContents(filledDir, originalCopy)
      }
      "Can rename directory" in {
        targetDir.addSubDir(filledDir.name)
        val newName = filledDir.name + "foo"
        val movedDir = RichFileUtils.move(filledDir, targetDir, newName)
        filledDir.exists shouldReturn false
        movedDir.name shouldReturn newName
        assertSameContents(originalCopy, movedDir)
      }
    }
    "move directory's contents" - {
      "happy path" in {
        RichFileUtils.moveContents(filledDir, targetDir)
        assertEmptyDir(filledDir)
        assertSameContents(originalCopy, targetDir)
      }
      "a file already exists with name" in {
        val sameFile = targetDir.addFile(filledDir.files.next.name)
        an[FileAlreadyExistsException] should be thrownBy RichFileUtils.moveContents(
          filledDir,
          targetDir,
        )
        assertSameContents(originalCopy, filledDir)
        assert(sameFile.delete())
        assertEmptyDir(targetDir)
      }
    }
    "rename directory" - {
      "happy path" in {
        val movedDir = RichFileUtils.rename(filledDir, filledDir.name + "foo")
        movedDir.name shouldReturn (originalName + "foo")
        assertSameContents(movedDir, originalCopy)
      }
      "dir with same name already exists" in {
        filledDir.parent.addSubDir("foobar")
        an[FileAlreadyExistsException] should be thrownBy RichFileUtils.rename(filledDir, "foobar")
        assertSameContents(filledDir, originalCopy)
      }
    }
  }
}
