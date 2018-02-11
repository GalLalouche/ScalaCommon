package common

import common.rich.RichT._
import common.rich.collections.RichTraversableOnce._
import common.rich.path.RichFile._
import common.rich.path.{Directory, RichFile, RichPath, TempDirectory}
import org.scalatest.{BeforeAndAfter, Suite}

/**
 * Several helping methods and fixtures for testing classes that works with IO
 */
trait DirectorySpecs extends AuxSpecs with BeforeAndAfter {
  self: Suite =>
  after {
    if (tempDir.exists())
      tempDir.clear()
  }
  val tempDir = TempDirectory()
  lazy val tempFile = tempDir addFile "tempFile"
  lazy val filledDir: Directory = {
    val $ = tempDir addSubDir "foobar"
    $ addFile "foobar" write "stuff"
    val subDir = $ addSubDir "moo"
    subDir addFile "moof" write "some more stuff"
    subDir addFile "blah" write "even more stuff"
    $
  }

  def assertSameContents(dir1: Directory, dir2: Directory, assertSameName: Boolean = false): Unit = {
    if (assertSameName) assert(dir1.name == dir2.name, "Directories have different names")
    def mapByName[P <: RichPath[_]](ps: TraversableOnce[P]): Map[String, P] =
      ps.mapBy(_.name)
    val dir1Files = dir1.files
    val dir2Files = dir1.files map RichFile.apply mapTo mapByName
    assert(dir1Files.size == dir2Files.size,
      s"dir1 <$dir1> doesn't have the same number of files as <$dir2>")
    dir1Files.foreach(dir1File => {
      val file1Name = dir1File.name
      val dir2File = dir2Files get file1Name
      assert(dir2File.isDefined, s"<$dir2> has no file with name <$file1Name>")
      assert(dir1File.hasSameContentAs(dir2File.get),
        s"file with name <$file1Name> has different contents in dirs <$dir1>, <$dir2>")
    })

    val dir1Subdirs = dir1.dirs
    val dir2Subdirs = dir2.dirs |> mapByName
    assert(dir1Subdirs.size == dir2Subdirs.size,
      s"dir1 <$dir1> doesn't have the same number of sub directories as <$dir2>")
    dir1Subdirs.foreach(dir1Subdir => {
      val subdir1Name = dir1Subdir.name
      val dir2Subdir = dir2Subdirs get subdir1Name
      assert(dir2Subdir.isDefined, s"<$dir2> has no sub directory with name <$subdir1Name>")
      assertSameContents(dir1Subdir, dir2Subdir.get, assertSameName = true)
    })
 }
}
