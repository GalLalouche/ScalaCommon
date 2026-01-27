package common.rich.path

import java.io.{File, IOException}
import java.nio.file.{Files, FileVisitResult, Path, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes

import rx.lang.scala.{Observable, Subscriber}

import common.rich.RichT.richT
import common.rich.primitives.RichBoolean._

/** Helper class for Directory methods. */
class Directory private[path] (val dir: File) extends RichPath(dir) {
  def listFiles: Seq[File] = dir.listFiles

  /** Adds a new file under the directory if one doesn't exist, and returns it. */
  def addFile(name: String): File = {
    val $ = new File(dir, name)
    $.createNewFile()
    $
  }
  /** Adds a new sub-directory under this directory if one doesn't exist, and returns it. */
  def addSubDir(name: String): Directory = {
    val $ = new File(dir, name)
    $.mkdir()
    new Directory($)
  }

  /** Returns all direct sub-directory of this directory. */
  def dirs: Iterator[Directory] = dir.listFiles.iterator.filter(_.isDirectory).map(new Directory(_))

  /** All direct files of this directory, that are *not* directories */
  def files: Iterator[File] = listFiles.iterator.filterNot(_.isDirectory)

  /** Deletes all files and directories in this dir recursively including itself. */
  def deleteAll() {
    def deleteAll(d: Directory) {
      d.dirs.foreach(deleteAll)
      d.files.foreach(x => if (x.exists && x.delete.isFalse) println("could not delete: " + x))
      if (d.dir.exists && d.dir.delete.isFalse)
        println("could not delete: " + d.dir)
    }
    deleteAll(this)
  }
  /** Deletes all files and directories in this dir recursively <b>not</b> including itself. */
  def clear(): Directory = {
    files.foreach(_.delete)
    dirs.foreach(_.deleteAll())
    this
  }
  /** Significantly faster than the above iterator (at least on Windows). */
  def deepFiles: Iterator[File] = deepPaths.filter(_.isFile)
  /** Faster than the above iterator (at least on Windows). */
  def deepFilesObservable: Observable[(File, BasicFileAttributes)] = observable { sub =>
    new EvenSimplerVisitor[Path](sub) {
      protected override def onFile(file: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((file.toFile, attrs))
    }
  }
  /** Returns all directories nested inside this directory (in any given depth). */
  def deepDirs: Iterator[Directory] =
    deepPaths.flatMap(new Directory(_).optFilter(_.isDirectory))
  /** Significantly faster than the above iterator (at least on Windows). */
  def deepDirsObservable: Observable[(Directory, BasicFileAttributes)] = observable { sub =>
    new EvenSimplerVisitor[Directory](sub) {
      protected override def onDirectory(dir: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((Directory.unsafe(dir.toFile), attrs))
    }
  }

  /** Returns all files and directories nested inside this directory (in any given depth). */
  def deepPaths: Iterator[File] = dir.listFiles.iterator.flatMap { f =>
    Iterator(f).mapIf(f.isDirectory).to(_ ++ new Directory(f).deepPaths)
  }
  /** Significantly faster than the above iterator (at least on Windows). */
  def deepPathsObservable: Observable[(File, BasicFileAttributes)] = observable(sub =>
    new EvenSimplerVisitor[Path](sub) {
      protected override def onFile(file: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((file.toFile, attrs))
      protected override def onDirectory(dir: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((dir.toFile, attrs))
    },
  )

  private def observable[A](
      fromSub: Subscriber[(A, BasicFileAttributes)] => SimpleFileVisitor[Path],
  ): Observable[(A, BasicFileAttributes)] = Observable { sub =>
    try {
      Files.walkFileTree(dir.toPath, fromSub(sub))
      sub.onCompleted()
    } catch {
      case e: Throwable => sub.onError(e)
    }
  }

  override def toString = s"Directory($dir)"

  private abstract class EvenSimplerVisitor[A](sub: Subscriber[_]) extends SimpleFileVisitor[Path] {
    protected def onFile(file: Path, attrs: BasicFileAttributes): Unit = {}
    protected def onDirectory(dir: Path, attrs: BasicFileAttributes): Unit = {}
    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (file ne dir.toPath)
        onFile(file, attrs)
      result
    }
    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (dir ne Directory.this.dir.toPath)
        onDirectory(dir, attrs)
      result
    }
    override def visitFileFailed(file: Path, exc: IOException) = {
      sub.onError(exc)
      super.visitFileFailed(file, exc)
    }
    private def result: FileVisitResult =
      if (sub.isUnsubscribed) FileVisitResult.TERMINATE else FileVisitResult.CONTINUE
  }
}

object Directory {
  def apply(f: File): Directory = {
    require(f.exists && f.isDirectory, s"${f.getAbsolutePath} is not a directory")
    new Directory(f)
  }
  def apply(s: String): Directory = apply(new File(s))
  /** Does not check for existence or that the file is a directory. */
  @inline def unsafe(s: String): Directory = unsafe(new File(s))
  @inline def unsafe(f: File): Directory = new Directory(f)
  /** Creates all directories along the path as needed */
  def makeDir(f: File): Directory = {
    if (f.isDirectory.isFalse)
      require(f.mkdirs(), "Could not create directories path " + f)
    new Directory(f)
  }

  def makeDir(fullPath: String): Directory = makeDir(new File(fullPath))
}
