package common.rich.path

import org.scalatest.OneInstancePerTest
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable

import common.rich.path.ObservableRichFileUtils.MoveFileProgress
import common.rx.report.ReportObserver
import common.test.{AuxSpecs, DirectorySpecs}

class ObservableRichFileUtilsTest
    extends AnyFreeSpec
    with DirectorySpecs
    with AuxSpecs
    with OneInstancePerTest {
  private lazy val dstDir = TempDirectory()

  "moveContents" - {
    "empty directory" in {
      val emptySrcDir = tempDir.addSubDir("emptySrc")
      val emptyDstDir = dstDir.addSubDir("emptyDst")

      val steps = mutable.Buffer[MoveFileProgress]()
      var completed = false
      var error: Option[Throwable] = None

      val observer = new ReportObserver[MoveFileProgress, Unit] {
        override def onStep(a: MoveFileProgress): Unit = steps += a
        override def onComplete(r: Unit): Unit = completed = true
        override def onError(t: Throwable): Unit = error = Some(t)
      }

      ObservableRichFileUtils.moveContents(emptySrcDir, emptyDstDir, observer)

      emptySrcDir.listFiles shouldBe empty
      emptyDstDir.listFiles shouldBe empty
      steps shouldBe empty
      completed shouldReturn true
      error shouldBe empty
    }

    "directory with 3 files" in {
      val srcDir = tempDir.addSubDir("srcDir")
      srcDir.addFile("file1.txt").write("content1")
      srcDir.addFile("file2.txt").write("content2")
      srcDir.addFile("file3.txt").write("content3")

      val dstDir = this.dstDir.addSubDir("dstDir")

      val steps = mutable.Buffer[MoveFileProgress]()
      var completed = false
      var error: Option[Throwable] = None

      val observer = new ReportObserver[MoveFileProgress, Unit] {
        override def onStep(a: MoveFileProgress): Unit = steps += a
        override def onComplete(r: Unit): Unit = completed = true
        override def onError(t: Throwable): Unit = error = Some(t)
      }

      ObservableRichFileUtils.moveContents(srcDir, dstDir, observer)

      srcDir.listFiles shouldBe empty
      dstDir.files.size shouldReturn 3
      dstDir.files.map(_.getName).shouldContainExactly("file1.txt", "file2.txt", "file3.txt")
      RichFile(dstDir \ "file1.txt").readAll shouldReturn "content1"
      RichFile(dstDir \ "file2.txt").readAll shouldReturn "content2"
      RichFile(dstDir \ "file3.txt").readAll shouldReturn "content3"
      steps.size shouldReturn 3
      steps.map(_.current.getName).shouldContainExactly("file1.txt", "file2.txt", "file3.txt")
      steps.map(_.total).distinct.toVector shouldReturn Vector(3)
      steps.map(_.processed).toVector shouldReturn Vector(1, 2, 3)
      completed shouldReturn true
      error shouldBe empty
    }

    "nested directory structure" in {
      val srcDir = tempDir.addSubDir("srcDir")
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

      val dstDir = this.dstDir.addSubDir("dstDir")

      val steps = mutable.Buffer[MoveFileProgress]()
      var completed = false
      var error: Option[Throwable] = None

      val observer = new ReportObserver[MoveFileProgress, Unit] {
        override def onStep(a: MoveFileProgress): Unit = steps += a
        override def onComplete(r: Unit): Unit = completed = true
        override def onError(t: Throwable): Unit = error = Some(t)
      }

      ObservableRichFileUtils.moveContents(srcDir, dstDir, observer)

      srcDir.files shouldBe empty
      srcDir.dirs shouldBe empty
      dstDir.files.map(_.getName).shouldContainExactly("rootFile.txt")
      RichFile(dstDir \ "rootFile.txt").readAll shouldReturn "rootContent"
      dstDir.dirs.map(_.name).shouldContainExactly("dir1", "dir2")
      val movedDir1 = Directory(dstDir \ "dir1")
      movedDir1.files shouldBe empty
      movedDir1.dirs.map(_.name).shouldContainExactly("subdir1", "subdir2")
      val movedSubdir1 = Directory(movedDir1 \ "subdir1")
      movedSubdir1.files.map(_.getName).shouldContainExactly("file1.txt")
      RichFile(movedSubdir1 \ "file1.txt").readAll shouldReturn "content1"
      val movedSubdir2 = Directory(movedDir1 \ "subdir2")
      movedSubdir2.files.map(_.getName).shouldContainExactly("file2.txt", "file3.txt")
      RichFile(movedSubdir2 \ "file2.txt").readAll shouldReturn "content2"
      RichFile(movedSubdir2 \ "file3.txt").readAll shouldReturn "content3"
      val movedDir2 = Directory(dstDir \ "dir2")
      movedDir2.files.map(_.getName).shouldContainExactly("file4.txt", "file5.txt", "file6.txt")
      RichFile(movedDir2 \ "file4.txt").readAll shouldReturn "content4"
      RichFile(movedDir2 \ "file5.txt").readAll shouldReturn "content5"
      RichFile(movedDir2 \ "file6.txt").readAll shouldReturn "content6"
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
