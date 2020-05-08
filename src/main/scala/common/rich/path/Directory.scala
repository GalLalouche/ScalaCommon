package common.rich.path

import java.io.File
import common.rich.primitives.RichBoolean._

/** Helper class for Directory methods. */
class Directory(val dir: File) extends RichPath[Directory](dir) {
  require(dir.getAbsoluteFile.isDirectory, s"${dir.getAbsolutePath} is not a directory")

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
    Directory($)
  }

  /** Returns all direct sub-directory of this directory. */
  def dirs: Seq[Directory] =
    Option(dir.listFiles)
        .getOrElse(Array())
        .toVector
        .filter(_.isDirectory)
        .map(Directory(_))

  /** All direct files of this directory, that are *not* directories */
  def files: Seq[File] = listFiles.toVector.filterNot(_.isDirectory)

  /** Deletes all files and directories in this dir recursively including itself. */
  def deleteAll() {
    System.gc()
    def deleteAll(d: Directory) {
      System.gc()
      d.dirs.foreach(deleteAll)
      d.files.foreach(x => if (x.exists && x.delete.isFalse) println("could not delete: " + x))
      if (d.dir.exists && d.dir.delete.isFalse) {
        System.gc()
        println("could not delete: " + d.dir)
      }
    }
    deleteAll(this)
  }
  /** Deletes all files and directories in this dir recursively <b>not</b> including itself. */
  def clear(): Directory = {
    files.foreach(_.delete)
    dirs.foreach(_.deleteAll())
    this
  }
  /** Returns all files that are not directories nested inside this directory (in any given depth). */
  def deepFiles: Stream[File] = files.toStream ++ dirs.flatMap(_.deepFiles)
  /** Returns all directories nested inside this directory (in any given depth). */
  def deepDirs: Stream[Directory] = dirs.toStream ++ dirs.flatMap(_.deepDirs)
  /** Returns all files and directories nested inside this directory (in any given depth). */
  def deepPaths: Stream[File] = files.toStream ++ dirs.flatMap(_.deepPaths)

  /**
   * Clones the directory, creating a copy of it suffixed with "clone" by default.
   * This will override any other cloned directory
   */
  def cloneDir(suffix: String = "_clone"): Directory = {
    require(suffix.nonEmpty, "Must provide a suffix when cloning")
    val newName = name + suffix
    parent.addSubDir(newName).deleteAll() // delete previous directory if it exists
    copyTo(parent, newName)
  }

  override protected def internalCopyTo(f: File): Directory = {
    import RichFile._
    f.mkdir()
    val $ = Directory(f)
    files.foreach(_.copyTo($))
    dirs.foreach(_.copyTo($))
    Directory($)
  }
}

object Directory {
  def apply(f: File): Directory = new Directory(f)
  def apply(s: String): Directory = Directory(new File(s))
  /** Creates all directories along the path as needed */
  def makeDir(f: File): Directory = {
    if (f.isDirectory.isFalse)
      require(f.mkdirs(), "Could not create directories path " + f)
    Directory(f)
  }

  def makeDir(fullPath: String): Directory = makeDir(new File(fullPath))
}
