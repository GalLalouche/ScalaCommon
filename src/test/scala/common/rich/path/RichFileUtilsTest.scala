package common.rich.path

import java.nio.file.FileAlreadyExistsException

import common.DirectorySpecs
import common.rich.collections.RichTraversableOnce._
import RichFile._
import org.scalatest.{FreeSpec, OneInstancePerTest}

class RichFileUtilsTest extends FreeSpec with DirectorySpecs with OneInstancePerTest {
  lazy val dir2 = TempDirectory()
  "move file" - {
    tempFile write "foobar"
    val tempFileName = tempFile.name
    val otherFile = tempDir addFile "other_file"
    otherFile write "bazz"
    def verifyFile(f: RichFile, name: String = tempFileName): Unit = {
      f.exists shouldReturn true
      f.readAll shouldReturn "foobar"
      f.name shouldReturn name
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
      a[FileAlreadyExistsException] should be thrownBy RichFileUtils.move(tempFile, dir2, "new_name.txt")
      verifyNoChange()
    }
    "within same directory" - {
      "happy path" in {
        val newFile = RichFileUtils.move(tempFile, tempFileName + "foo")
        tempFile.exists shouldReturn false
        verifyFile(newFile, tempFileName + "foo")
      }
      "same name" in {
        RichFileUtils.move(tempFile, tempFileName)
        verifyNoChange()
      }
      "file with name already exists" in {
        a[FileAlreadyExistsException] should be thrownBy RichFileUtils.move(tempFile, otherFile.name)
        verifyNoChange()
      }
    }
  }
  "directory movers" - {
    val srcDir: Directory = TempDirectory()
    val dirName = srcDir.name
    srcDir.addFile("foo.txt").write("some stuff")
    srcDir.addSubDir("bar").addFile("bar.txt").write("some other stuff")

    def verifyStructure(d: Directory): Unit = {
      d.exists shouldReturn true
      val movedFooTxtFile = d.files.single
      movedFooTxtFile.name shouldReturn "foo.txt"
      movedFooTxtFile.readAll shouldReturn "some stuff"
      val movedBarDir = d.dirs.single
      movedBarDir.name shouldReturn "bar"
      val movedBarTxtFile = movedBarDir.files.single
      movedBarTxtFile.name shouldReturn "bar.txt"
      movedBarTxtFile.readAll shouldReturn "some other stuff"
    }

    "move directory" - {
      "happy path" in {
        val parentDir: Directory = TempDirectory()
        RichFileUtils.move(srcDir, parentDir)
        srcDir.exists shouldReturn false
        verifyStructure(parentDir / dirName /)
      }
      "Existing directory with same name throws" in {
        dir2.addSubDir(srcDir.name)
        an[FileAlreadyExistsException] should be thrownBy RichFileUtils.move(srcDir, dir2)
        verifyStructure(srcDir)
      }
      "Can rename directory" in {
        dir2.addSubDir(srcDir.name)
        val newName = srcDir.name + "foo"
        RichFileUtils.move(srcDir, dir2, newName)
        verifyStructure(dir2 / newName /)
      }
    }
    "move directory's contents" - {
      "happy path" in {
        RichFileUtils.moveContents(srcDir, dir2)
        verifyStructure(dir2)
      }
      "a file already exists with name" in {
        dir2.addFile("foo.txt").write("blah blah")
        an[FileAlreadyExistsException] should be thrownBy RichFileUtils.moveContents(srcDir, dir2)
        verifyStructure(srcDir)
        dir2.dirs shouldBe 'empty
        val preExistingFile = dir2.files.single
        preExistingFile.name shouldReturn "foo.txt"
        preExistingFile.readAll shouldReturn "blah blah"
      }
    }
  }
}
