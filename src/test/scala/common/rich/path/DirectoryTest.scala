package common.rich.path

import java.io.File

import org.scalatest.{BeforeAndAfter, OneInstancePerTest}
import org.scalatest.freespec.AnyFreeSpec

import scala.language.postfixOps

import common.rich.path.RichFile.richFile
import common.rich.path.ref.io.IODirectory
import common.rx.RichObservable.richObservable
import common.test.DirectorySpecs

class DirectoryTest
    extends AnyFreeSpec
    with DirectorySpecs
    with OneInstancePerTest
    with BeforeAndAfter {
  private def setupDir(dir: IODirectory): Unit = {
    dir.addFile("file1.txt")
    dir.addFile("file2.txt")

    val dir1 = dir.addSubDir("dir1")
    val dir2 = dir.addSubDir("dir2")

    dir1.addFile("file3.txt")

    dir2.addSubDir("subdir").addFile("file4.txt")
  }

  private def testDeepFiles(dir: IODirectory, f: IODirectory => Seq[File]): Unit = {
    setupDir(dir)
    f(dir).toSet shouldReturn Set(
      tempDir / "file1.txt",
      tempDir / "file2.txt",
      tempDir / "dir1" / "file3.txt",
      tempDir / "dir2" / "subdir" / "file4.txt",
    )
  }

  private def testDeepDirs(dir: IODirectory, f: IODirectory => Seq[IODirectory]): Unit = {
    setupDir(dir)
    f(dir).toSet shouldReturn Set(
      IODirectory(new File(tempDir, "dir1")),
      IODirectory(new File(tempDir, "dir2")),
      IODirectory(tempDir / "dir2" / "subdir"),
    )
  }

  private def testDeepPaths(dir: IODirectory, f: IODirectory => Seq[File]): Unit = {
    setupDir(dir)
    f(dir).toSet shouldReturn Set(
      new File(tempDir, "file1.txt"),
      new File(tempDir, "file2.txt"),
      new File(tempDir, "dir1"),
      tempDir / "dir1" / "file3.txt",
      new File(tempDir, "dir2"),
      tempDir / "dir2" / "subdir",
      tempDir / "dir2" / "subdir" / "file4.txt",
    )
  }
  // yeah yeah, it uses DirectorySpecs which uses Directory

  "Ctor should" - {
    "throw exception" - {
      "if file does not exist" in {
        an[IllegalArgumentException] should be thrownBy {
          IODirectory("C:/__this_should_not_Ever_EXIST_!@#!@#!13123123")
        }
      }
      "if file isn't a directory" in {
        val f = tempDir.addFile("file")
        an[IllegalArgumentException] should be thrownBy {
          IODirectory(f)
        }
      }
    }
  }

  "Directory should" - {
    val $ = tempDir
    "add" - {
      "file" in {
        $.addFile("foo.bar")
        new File($, "foo.bar") should exist
      }
      "directory" in {
        $.addSubDir("foobar")
        new File($, "foobar").isDirectory shouldReturn true
      }
    }
    "list files" - {
      "nothing when no files" in {
        $.files.isEmpty shouldReturn true
      }
      "list all files" in {
        $.addFile("foo.bar")
        $.addFile("bar.foo")
        $.files.toSet === Set(new File(tempDir, "foo.bar"), new File(tempDir, "bar.foo"))
      }
      "not list deep files" in {
        $.addSubDir("foo").addFile("bar")
        $.files.isEmpty shouldReturn true
      }
    }
    "list dirs" - {
      "nothing when no dirs" in {
        $.files.isEmpty shouldReturn true
      }
      "list all dirs" in {
        $.addSubDir("foobar")
        $.addSubDir("barfoo")
        $.dirs.toSet === Set(new File(tempDir, "foobar"), new File(tempDir, "barfoo"))
          .map(IODirectory(_))
      }
      "not list deep dirs" in {
        $.addSubDir("foo").addSubDir("bar")
        $.dirs === Vector(IODirectory(new File(tempDir, "foo")))
      }
    }
    "deep" - {
      "deepFiles" in {
        testDeepFiles($, _.deepFiles.toSeq)
      }
      "deepFilesObservable" in {
        testDeepFiles($, _.deepFilesObservable.map(_._1).toVectorBlocking)
      }
      "deepDirs" in {
        testDeepDirs($, _.deepDirs.toSeq)
      }
      "deepDirsObservable" in {
        testDeepDirs(
          $,
          _.deepDirsObservable.map(_._1).toVectorBlocking,
        )
      }
      "deepPaths" in {
        testDeepPaths($, _.deepPaths.map(_.asInstanceOf[File]).toVector)
      }
      "deepPathsObservable" in {
        testDeepPaths($, _.deepPathsObservable.map(_._1).toVectorBlocking)
      }
    }
    "clear" - {
      "not delete self" in {
        $.clear()
        $ should exist
      }
      "delete all" - {
        "only files" in {
          $.addFile("foo.bar")
          $.addFile("bar.foo")
          $.clear()
          $.files.isEmpty shouldReturn true
        }
        "only dirs" in {
          $.addSubDir("foobar")
          $.addSubDir("barfoo")
          $.clear()
          $.files.isEmpty shouldReturn true
        }
        "recursive" in {
          $.addFile("foo.bar")
          $.addSubDir("foobar").addFile("bar.foo")
          $.clear()
          $.files.isEmpty shouldReturn true
        }
      }
    }
    "delete should delete self" in {
      $.addFile("foo.bar")
      $.addSubDir("foobar").addFile("bar.foo")
      $.deleteAll()
      $ should not(exist)
    }
    "parent" - {
      "return all parent dirs" in {
        val c = tempDir.addSubDir("a").addSubDir("b").addSubDir("c")
        c.parent === tempDir / "a" / "b"
        c.parent.parent === tempDir / "a"
        c.parent.parent.parent === tempDir
      }
      "throw exception on root" in {
        an[UnsupportedOperationException] should be thrownBy {
          File.listRoots()(0).parent
        }
      }
    }
  }
}
