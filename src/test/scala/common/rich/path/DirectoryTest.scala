package common.rich.path

import java.io.File

import org.scalatest.{BeforeAndAfter, OneInstancePerTest}
import org.scalatest.freespec.AnyFreeSpec

import scala.language.postfixOps

import common.rich.path.RichPath._
import common.test.DirectorySpecs

class DirectoryTest
    extends AnyFreeSpec
    with DirectorySpecs
    with OneInstancePerTest
    with BeforeAndAfter {
  // yeah yeah, it uses DirectorySpecs which uses Directory

  "Ctor should" - {
    "throw exception" - {
      "if file does not exist" in {
        an[IllegalArgumentException] should be thrownBy {
          Directory("C:/__this_should_not_Ever_EXIST_!@#!@#!13123123")
        }
      }
      "if file isn't a directory" in {
        val f = tempDir.addFile("file")
        an[IllegalArgumentException] should be thrownBy {
          Directory(f)
        }
      }
    }
  }

  "Directory should" - {
    val $ = tempDir
    "add" - {
      "file" in {
        $.addFile("foo.bar")
        new File($.dir, "foo.bar") should exist
      }
      "directory" in {
        $.addSubDir("foobar")
        new File($.dir, "foobar").isDirectory shouldReturn true
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
          .map(Directory(_))
      }
      "not list deep dirs" in {
        $.addSubDir("foo").addSubDir("bar")
        $.dirs === Vector(Directory(new File(tempDir, "foo")))
      }
    }
    "list deep files both deep and shallow" in {
      $.addFile("foo.bar")
      $.addSubDir("foobar").addFile("bar.foo")
      $.deepFiles.toSet === Set(
        new File(tempDir, "foo.bar"),
        new File(tempDir / "foobar" /, "bar.foo"),
      )
    }
    "list deep dirs both deep and shallow" in {
      $.addSubDir("foobar").addSubDir("barfoo")
      $.deepDirs.map(_.dir).toSet === Set(
        new File(tempDir, "foobar"),
        new File((tempDir / "foobar" / "barfoo" /).path),
      )
    }
    "list deep paths both deep and shallow" in {
      $.addSubDir("foobar").addFile("bar.foo")
      $.deepPaths.toSet === Set(
        new File(tempDir, "foobar"),
        new File((tempDir / "foobar" / "bar.foo").path),
      )
    }
    "clear" - {
      "not delete self" in {
        $.clear()
        $.dir should exist
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
      $.dir should not(exist)
    }
    "parent" - {
      "return all parent dirs" in {
        val c = tempDir.addSubDir("a").addSubDir("b").addSubDir("c")
        c.parent === (tempDir / "a" / "b" /)
        c.parent.parent === (tempDir / "a" /)
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
