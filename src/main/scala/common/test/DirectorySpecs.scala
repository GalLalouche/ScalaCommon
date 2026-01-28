package common.test

import java.io.File

import org.scalatest.{BeforeAndAfter, Suite}

import common.rich.RichT._
import common.rich.collections.RichMap.richMap
import common.rich.collections.RichTraversableOnce._
import common.rich.path.RichFile.richFile
import common.rich.path.ref.io.{IODirectory, TempDirectory}

/** Several helping methods and fixtures for testing classes that works with IO */
trait DirectorySpecs extends AuxSpecs with BeforeAndAfter { self: Suite =>
  protected val tempDir: TempDirectory = TempDirectory()
  after {
    if (tempDir.exists())
      tempDir.clear()
  }
  protected lazy val tempFile: File = tempDir.addFile("tempFile")
  protected lazy val filledDir: IODirectory = {
    val $ = tempDir.addSubDir("foobar")
    $.addFile("foobar").write("stuff")
    val subDir = $.addSubDir("moo")
    subDir.addFile("moof").write("some more stuff")
    subDir.addFile("blah").write("even more stuff")
    $
  }

  def assertSameContents(
      dir1: IODirectory,
      dir2: IODirectory,
      assertSameName: Boolean = false,
  ): Unit = {
    if (assertSameName) assert(dir1.name == dir2.name, "Directories have different names")
    def mapByName(ps: TraversableOnce[File]): Map[String, File] =
      ps.mapBy(_.getName)
    val dir1Files = dir1.files
    val dir2Files = dir1.files.thrush(mapByName)
    assert(
      dir1Files.size == dir2Files.size,
      s"dir1 <$dir1> doesn't have the same number of files as <$dir2>",
    )
    dir1Files.foreach { dir1File =>
      val file1Name = dir1File.name
      val dir2File = dir2Files.get(file1Name)
      assert(dir2File.isDefined, s"<$dir2> has no file with name <$file1Name>")
      assert(
        dir1File.hasSameContentAs(dir2File.get),
        s"file with name <$file1Name> has different contents in dirs <$dir1>, <$dir2>",
      )
    }

    val dir1Subdirs = dir1.dirs
    val dir2Subdirs = mapByName(dir2.dirs).properMapValues(_.asInstanceOf[IODirectory])
    assert(
      dir1Subdirs.size == dir2Subdirs.size,
      s"dir1 <$dir1> doesn't have the same number of sub directories as <$dir2>",
    )
    dir1Subdirs.foreach { dir1Subdir =>
      val subdir1Name = dir1Subdir.name
      val dir2Subdir = dir2Subdirs.get(subdir1Name)
      assert(dir2Subdir.isDefined, s"<$dir2> has no sub directory with name <$subdir1Name>")
      assertSameContents(dir1Subdir, dir2Subdir.get, assertSameName = true)
    }
  }
}
