package common.rich.path

import java.io.File

import common.rich.RichT.richT
import common.rich.primitives.RichBoolean._

/** Helper class for Directory methods. */
class Directory private[path] (val dir: File) extends RichPath(dir) {
  def listFiles: Seq[File] = dir.listFiles

  /** Adds a new file under the directory if one doesn't exist, and returns it. */
  def addFile(name: String): RichFile = {
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
  /**
   * Returns all files that are not directories nested inside this directory (in any given depth).
   */
  def deepFiles: Iterator[File] = deepPaths.filter(_.isFile)
  /** Returns all directories nested inside this directory (in any given depth). */
  def deepDirs: Iterator[Directory] =
    deepPaths.flatMap(new Directory(_).optFilter(_.isDirectory))

  /** Returns all files and directories nested inside this directory (in any given depth). */
  def deepPaths: Iterator[File] =
    dir.listFiles.iterator.flatMap { f =>
      Iterator(f).mapIf(f.isDirectory).to(_ ++ new Directory(f).deepPaths)
    }

  override def toString = s"Directory($dir)"
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
