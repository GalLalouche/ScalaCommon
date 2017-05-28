package common.rich.path

import java.io.File
import java.nio.file.FileAlreadyExistsException

import common.DirectorySpecs
import RichFile._
import common.rich.collections.RichTraversableOnce._
import RichPath.poorPath
import org.scalatest.{BeforeAndAfter, FreeSpec, OneInstancePerTest}

class DirectoryTest extends FreeSpec with DirectorySpecs with OneInstancePerTest with BeforeAndAfter {
  // yeah yeah, it uses DirectorySpecs which uses Directory

  "Ctor should" - {
    "throw exception" - {
      "if file does not exist" in {
        an[IllegalArgumentException] should be thrownBy {Directory("C:/__this_should_not_Ever_EXIST_!@#!@#!13123123")}
      }
      "if file isn't a directory" in {
        val f = tempDir.addFile("file")
        an[IllegalArgumentException] should be thrownBy {Directory(f)}
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
        $.dirs.toSet === Set(new File(tempDir, "foobar"), new File(tempDir, "barfoo")).map(Directory(_))
      }
      "not list deep dirs" in {
        $.addSubDir("foo").addSubDir("bar")
        $.dirs === List(Directory(new File(tempDir, "foo")))
      }
    }
    "list deep files both deep and shallow" in {
      $.addFile("foo.bar")
      $.addSubDir("foobar").addFile("bar.foo")
      $.deepFiles.toSet === Set(new File(tempDir, "foo.bar"), new File(tempDir / "foobar" /, "bar.foo"))
    }
    "list deep dirs both deep and shallow" in {
      $.addSubDir("foobar").addSubDir("barfoo")
      $.deepDirs.map(_.dir).toSet === Set(new File(tempDir, "foobar"), new File((tempDir / "foobar" / "barfoo" /).path))
    }
    "list deep paths both deep and shallow" in {
      $.addSubDir("foobar").addFile("bar.foo")
      $.deepPaths.map(_.p).toSet === Set(new File(tempDir, "foobar"), new File((tempDir / "foobar" / "bar.foo").path))
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
        an[UnsupportedOperationException] should be thrownBy {File.listRoots()(0).parent}
      }
    }
    "Clone dir" - {
      val subDir = $ addSubDir "subDir"
      "create a new directory" in {
        val c = subDir.cloneDir()
        c should not be tempDir
      }
      "add files" in {
        subDir addFile "foo"
        val c = subDir.cloneDir()
        c.files.map(_.name) shouldReturn Seq("foo")
      }
      "add subdirs" in {
        subDir addSubDir "foo"
        val c = subDir.cloneDir()
        c.dirs.map(_.name) shouldReturn Seq("foo")
      }
      "add recursive files and dirs" in {
        subDir addSubDir "foo" addFile "bar"
        val c = subDir.cloneDir()
        (c / "foo" / "bar").p should exist
      }
      "overwrite existing directory" in {
        subDir.cloneDir() addFile "foo"
        subDir.cloneDir().files shouldBe empty
      }
    }
    "copyTo" - {
      // TODO merge with cloneDir tests
      // TODO figure a way to remove duplication with RichFileUtilsTests
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

      "throws an exception when a file already exists" in {
        val newDir = TempDirectory()
        a[FileAlreadyExistsException] should be thrownBy newDir.copyTo(srcDir.parent, srcDir.name)
        verifyStructure(srcDir)
      }

      "copy the entire directory" in {
        val newDir = srcDir.copyTo(tempDir)
        verifyStructure(newDir)
      }
    }
  }
}
