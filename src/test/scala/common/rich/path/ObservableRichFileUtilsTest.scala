package common.rich.path

import org.scalatest.OneInstancePerTest
import org.scalatest.freespec.AnyFreeSpec
import rx.lang.scala.Observer

import scala.collection.mutable

import common.rich.path.ObservableRichFileUtils.MoveFileProgress
import common.rich.path.RichFile.richFile
import common.test.{AuxSpecs, DirectorySpecs}

class ObservableRichFileUtilsTest
    extends AnyFreeSpec
    with DirectorySpecs
    with AuxSpecs
    with OneInstancePerTest {
  private val srcDir = tempDir.addSubDir("srcDir")
  private val dstDir = tempDir.addSubDir("dstDir")
  private val observer = new Observer[MoveFileProgress] {
    override def onNext(a: MoveFileProgress): Unit = steps += a
    override def onCompleted(): Unit = completed = true
    override def onError(t: Throwable): Unit = error = Some(t)
  }

  private val steps: mutable.Buffer[MoveFileProgress] = mutable.Buffer()
  private var completed: Boolean = false
  private var error: Option[Throwable] = None

  "moveContents" - {
    "empty directory" in {
      ObservableRichFileUtils.moveContents(srcDir, dstDir, observer)

      srcDir.listFiles shouldBe empty
      dstDir.listFiles shouldBe empty
      steps shouldBe empty
      completed shouldReturn true
      error shouldBe empty
    }

    "directory with 3 files" in {
      srcDir.addFile("file1.txt").write("content1")
      srcDir.addFile("file2.txt").write("content2")
      srcDir.addFile("file3.txt").write("content3")

      ObservableRichFileUtils.moveContents(srcDir, dstDir, observer)

      srcDir.listFiles shouldBe empty
      dstDir.files.size shouldReturn 3
      dstDir.files
        .map(_.getName)
        .toVector
        .shouldContainExactly("file1.txt", "file2.txt", "file3.txt")
      (dstDir \ "file1.txt").readAll shouldReturn "content1"
      (dstDir \ "file2.txt").readAll shouldReturn "content2"
      (dstDir \ "file3.txt").readAll shouldReturn "content3"
      steps
        .map(_.current.getName)
        .shouldContainExactly("file1.txt", "file2.txt", "file3.txt")
      steps.map(_.total).distinct.toVector shouldReturn Vector(3)
      steps.map(_.processed).toVector shouldReturn Vector(1, 2, 3)
      completed shouldReturn true
      error shouldBe empty
    }

    "nested directory structure" in {
      val dir1 = srcDir.addSubDir("dir1")
      dir1.addSubDir("subdir1").addFile("file1.txt").write("content1")
      val subdir2 = dir1.addSubDir("subdir2")
      subdir2.addFile("file2.txt").write("content2")
      subdir2.addFile("file3.txt").write("content3")
      val dir2 = srcDir.addSubDir("dir2")
      dir2.addFile("file4.txt").write("content4")
      dir2.addFile("file5.txt").write("content5")
      dir2.addFile("file6.txt").write("content6")
      srcDir.addFile("rootFile.txt").write("rootContent")

      ObservableRichFileUtils.moveContents(srcDir, dstDir, observer)

      srcDir.files shouldBe empty
      srcDir.dirs shouldBe empty
      dstDir.files.map(_.getName).toVector.shouldContainExactly("rootFile.txt")
      (dstDir \ "rootFile.txt").readAll shouldReturn "rootContent"
      dstDir.dirs.map(_.name).toVector.shouldContainExactly("dir1", "dir2")
      val movedDir1 = Directory(dstDir \ "dir1")
      movedDir1.files shouldBe empty
      movedDir1.dirs.map(_.name).toVector.shouldContainExactly("subdir1", "subdir2")
      val movedSubdir1 = Directory(movedDir1 \ "subdir1")
      movedSubdir1.files.map(_.getName).toVector.shouldContainExactly("file1.txt")
      (movedSubdir1 \ "file1.txt").readAll shouldReturn "content1"
      val movedSubdir2 = Directory(movedDir1 \ "subdir2")
      movedSubdir2.files.map(_.getName).toVector.shouldContainExactly("file2.txt", "file3.txt")
      (movedSubdir2 \ "file2.txt").readAll shouldReturn "content2"
      (movedSubdir2 \ "file3.txt").readAll shouldReturn "content3"
      val movedDir2 = Directory(dstDir \ "dir2")
      movedDir2.files
        .map(_.getName)
        .toVector
        .shouldContainExactly("file4.txt", "file5.txt", "file6.txt")
      (movedDir2 \ "file4.txt").readAll shouldReturn "content4"
      (movedDir2 \ "file5.txt").readAll shouldReturn "content5"
      (movedDir2 \ "file6.txt").readAll shouldReturn "content6"
      steps
        .map(_.current.getName)
        .shouldContainExactly(
          "rootFile.txt",
          "file1.txt",
          "file2.txt",
          "file3.txt",
          "file4.txt",
          "file5.txt",
          "file6.txt",
        )
      steps.map(_.processed).toVector shouldReturn Vector(1, 2, 3, 4, 5, 6, 7)
      completed shouldReturn true
      error shouldBe empty
    }
  }
}

object ObservableRichFileUtilsTest {}
